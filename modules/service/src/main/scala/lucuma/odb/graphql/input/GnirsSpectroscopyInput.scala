// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import cats.syntax.unorderedFoldable.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Access
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.syntax.string.*
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

object GnirsSpectroscopyInput:

  // Signal-to-noise exposure time mode does not support coadds. When the ETM is set to
  // signal-to-noise, force coadds to 1 so a previously-set value doesn't linger.
  private def coaddsForEtm(
    etm:    Option[ExposureTimeMode],
    coadds: Option[PosInt]
  ): Option[PosInt] =
    etm match
      case Some(ExposureTimeMode.SignalToNoiseMode(_, _)) => PosInt.from(1).toOption
      case _                                           => coadds

  // The observing mode type follows the FPU: the long slit and the IFU are persisted
  // in the same table but carry distinct ObservingModeType values.
  private def modeTypeFor(fpu: GnirsFpu.Spectroscopy): ObservingModeType =
    fpu match
      case GnirsFpu.Spectroscopy.Slit(_) => ObservingModeType.GnirsLongSlit
      case GnirsFpu.Spectroscopy.Ifu(_)  => ObservingModeType.GnirsIfu

  // On create, exactly one of slit / ifu is required and must carry its FPU. The nested
  // structure makes a slit-config-with-ifu-FPU (and vice versa) mismatch impossible.
  private def resolveCreate(
    slit: Option[GnirsSlitInput.Value],
    ifu:  Option[GnirsIfuInput.Value]
  ): Result[(GnirsFpu.Spectroscopy, Option[SlitTelescopeConfigs], Option[NonEmptyList[TelescopeConfig]])] =
    (slit, ifu) match
      case (Some(s), None) =>
        Result.fromOption(s.fpu, Matcher.validationProblem("'slit.fpu' is required."))
          .map(f => (GnirsFpu.Spectroscopy.Slit(f), s.explicitTelescopeConfigs.toOption, None))
      case (None, Some(i)) =>
        Result.fromOption(i.fpu, Matcher.validationProblem("'ifu.fpu' is required."))
          .map(f => (GnirsFpu.Spectroscopy.Ifu(f), None, i.telescopeConfigs))
      case (None, None)    => Matcher.validationFailure("Exactly one of 'slit' or 'ifu' must be provided.")
      case _               => Matcher.validationFailure("Only one of 'slit' or 'ifu' may be provided.")

  // On edit, at most one of slit / ifu may be present. A missing telescopeConfigs (IFU) or
  // absent explicitTelescopeConfigs (slit) is left unedited.
  private def resolveEdit(
    slit: Option[GnirsSlitInput.Value],
    ifu:  Option[GnirsIfuInput.Value]
  ): Result[(Option[GnirsFpu.Spectroscopy], Nullable[SlitTelescopeConfigs], Option[NonEmptyList[TelescopeConfig]])] =
    (slit, ifu) match
      case (None, None)    => Result((None, Nullable.Absent, None))
      case (Some(s), None) => Result((s.fpu.map(GnirsFpu.Spectroscopy.Slit(_)), s.explicitTelescopeConfigs, None))
      case (None, Some(i)) => Result((i.fpu.map(GnirsFpu.Spectroscopy.Ifu(_)), Nullable.Absent, i.telescopeConfigs))
      case _               => Matcher.validationFailure("Only one of 'slit' or 'ifu' may be provided.")

  // GnirsSlitInput: fpu (required on create) + a clearable explicit telescope-config override.
  object GnirsSlitInput:
    case class Value(fpu: Option[GnirsFpuSlit], explicitTelescopeConfigs: Nullable[SlitTelescopeConfigs])
    val Binding: Matcher[Value] =
      ObjectFieldsBinding.rmap:
        case List(
          GnirsFpuSlitBinding.Option("fpu", rFpu),
          SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rTc)
        ) =>
          (rFpu, rTc).parMapN(Value.apply)

  // GnirsIfuInput: fpu (required on create) + telescope configs (missing = unedited; on
  // create a missing value is seeded from the FPU in the service).
  object GnirsIfuInput:
    case class Value(fpu: Option[GnirsFpuIfu], telescopeConfigs: Option[NonEmptyList[TelescopeConfig]])
    val Binding: Matcher[Value] =
      ObjectFieldsBinding.rmap:
        case List(
          GnirsFpuIfuBinding.Option("fpu", rFpu),
          TelescopeConfigInput.Binding.List.Option("telescopeConfigs", rTcList)
        ) =>
          (rFpu, rTcList).parTupled.flatMap: (fpu, tcList) =>
            tcList.traverse: cs =>
              NonEmptyList.fromList(cs).fold(
                Matcher.validationFailure("'telescopeConfigs' must not be empty")
              )(Result(_))
            .map(Value(fpu, _))

  case class AcquisitionInput(
    explicitFilter:   Nullable[GnirsFilter], // Nullable to allow clearing to the computed default
    explicitAcqType:  Nullable[GnirsAcquisitionType], // Nullable to allow clearing to automatic
    coadds:           Option[PosInt],
    skyOffset:        Option[Offset],
    exposureTimeMode: Option[ExposureTimeMode]
  )

  object AcquisitionInput:

    // A sky offset is valid exactly when the explicit acquisition type is FAINT:
    // FAINT requires one, and any other explicit type (or clearing to AUTO) forbids
    // it. This must hold within a single input; the DB also enforces it on the row.
    private def validateSkyOffset(a: AcquisitionInput): Result[AcquisitionInput] =
      val explicitlyFaint = a.explicitAcqType match
        case Nullable.NonNull(GnirsAcquisitionType.Faint) => true
        case _                                            => false
      (a.skyOffset.isDefined, explicitlyFaint) match
        case (true, false) =>
          OdbError.InvalidArgument("'skyOffset' is only valid when 'explicitAcquisitionType' is FAINT.".some).asFailure
        case (false, true) =>
          OdbError.InvalidArgument("'explicitAcquisitionType' FAINT requires a 'skyOffset'.".some).asFailure
        case _             =>
          Result(a)

    val Binding: Matcher[AcquisitionInput] =
      ObjectFieldsBinding.rmap:
        case List(
          GnirsFilterBinding.Nullable("explicitFilter", rFilter),
          GnirsAcquisitionTypeBinding.Nullable("explicitAcquisitionType", rAcqType),
          PosIntBinding.Option("coadds", rCoadds),
          OffsetInput.Binding.Option("skyOffset", rSkyOffset),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
        ) =>
          (
            rFilter.flatMap: n =>
              n.traverse: f =>
                if GnirsFilter.AcquisitionFilters.contains_(f) then f.success
                else OdbError.InvalidArgument(s"'explicitFilter' must contain one of: ${GnirsFilter.AcquisitionFilters.map(_.tag.toScreamingSnakeCase).mkString_(", ")}".some).asFailure
            ,
            rAcqType, rCoadds, rSkyOffset, rEtm
          ).parMapN(AcquisitionInput.apply)
           .map(a => a.copy(coadds = coaddsForEtm(a.exposureTimeMode, a.coadds)))
           .flatMap(validateSkyOffset)

  case class Create(
    exposureTimeMode: Option[ExposureTimeMode],
    coadds:           Option[PosInt],
    filter:           GnirsFilter,
    fpu:              GnirsFpu.Spectroscopy,
    camera:           GnirsCamera,
    grating:          GnirsGrating,
    prism:            GnirsPrism,
    centralWavelength:            Wavelength,
    explicitDecker:               Option[GnirsDecker]              = None,
    explicitGrating:              Option[GnirsGrating]             = None,
    explicitPrism:                Option[GnirsPrism]               = None,
    explicitFocusMotorSteps:      Option[Int]                      = None,
    explicitReadMode:             Option[GnirsReadMode]            = None,
    explicitWellDepth:            Option[GnirsWellDepth]           = None,
    explicitTelescopeConfigsSlit: Option[SlitTelescopeConfigs]     = None,
    telescopeConfigsIfu:          Option[NonEmptyList[TelescopeConfig]] = None,
    acquisition:                  Option[AcquisitionInput]         = None,
    telluricType:                 TelluricType                     = TelluricType.Hot
  ):
    def observingModeType: ObservingModeType = modeTypeFor(fpu)

    /** True if the input modifies fields that only Staff (or higher) may set. */
    def needsStaffAccess: Boolean = explicitFocusMotorSteps.isDefined

  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm),
          PosIntBinding.Option("coadds", rCoadds),
          GnirsFilterBinding("filter", rFilter),
          GnirsSlitInput.Binding.Option("slit", rSlit),
          GnirsIfuInput.Binding.Option("ifu", rIfu),
          GnirsCameraBinding("camera", rCamera),
          GnirsGratingBinding("grating", rGrating),
          GnirsPrismBinding("prism", rPrism),
          WavelengthInput.Binding("centralWavelength", rCentralWavelength),
          GnirsDeckerBinding.Option("explicitDecker", rDecker),
          GnirsGratingBinding.Option("explicitGrating", rExplGrating),
          GnirsPrismBinding.Option("explicitPrism", rExplPrism),
          IntBinding.Option("explicitFocusMotorSteps", rFocus),
          GnirsReadModeBinding.Option("explicitReadMode", rReadMode),
          GnirsWellDepthBinding.Option("explicitWellDepth", rWellDepth),
          AcquisitionInput.Binding.Option("acquisition", rAcq),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rEtm, rCoadds, rFilter, rSlit, rIfu, rCamera, rGrating, rPrism,
           rCentralWavelength, rDecker, rExplGrating, rExplPrism,
           rFocus, rReadMode, rWellDepth, rAcq, rTelluricType).parTupled.flatMap:
            (etm, coadds, filter, slit, ifu, camera, grating, prism,
             centralWavelength, decker, explGrating, explPrism,
             focus, readMode, wellDepth, acq, telluricType) =>
              resolveCreate(slit, ifu).map: (fpu, explTelescopeSlit, telescopeIfu) =>
                Create(etm, coaddsForEtm(etm, coadds), filter, fpu, camera, grating, prism,
                       centralWavelength, decker, explGrating, explPrism,
                       focus, readMode, wellDepth, explTelescopeSlit, telescopeIfu, acq,
                       telluricType.getOrElse(TelluricType.Hot))

  case class Edit(
    exposureTimeMode:          Option[ExposureTimeMode],
    coadds:                    Option[PosInt],
    filter:                    Option[GnirsFilter],
    fpu:                       Option[GnirsFpu.Spectroscopy],
    camera:                    Option[GnirsCamera],
    grating:                   Nullable[GnirsGrating],
    prism:                     Nullable[GnirsPrism],
    centralWavelength:         Option[Wavelength],
    explicitDecker:            Nullable[GnirsDecker],
    explicitGrating:           Nullable[GnirsGrating],
    explicitPrism:             Nullable[GnirsPrism],
    explicitFocusMotorSteps:   Nullable[Int],
    explicitReadMode:          Nullable[GnirsReadMode],
    explicitWellDepth:         Nullable[GnirsWellDepth],
    explicitTelescopeConfigsSlit: Nullable[SlitTelescopeConfigs], // Nullable to allow clearing to default
    telescopeConfigsIfu:          Option[NonEmptyList[TelescopeConfig]], // Option: set or skip (no clear; IFU always has a value)
    acquisition:               Option[AcquisitionInput],
    telluricType:              Option[TelluricType]            // Option: set or skip; cannot be unset
  ):
    def observingModeType: Option[ObservingModeType] = fpu.map(modeTypeFor)
    def updatesAcquisition: Boolean = acquisition.isDefined
    def limitToPreExecution(access: Access): Boolean = false

    /**
     * True if the input modifies fields that only Staff (or higher) may set.
     * Setting `explicitFocusMotorSteps` to a value requires Staff; clearing it
     * to null is allowed for anyone.
     */
    def needsStaffAccess: Boolean = explicitFocusMotorSteps.isPresent
    def toCreate: Result[Create] =
      def required[A](oa: Option[A], name: String): Result[A] =
        Result.fromOption(oa, Matcher.validationProblem(s"A $name is required to create a GNIRS spectroscopy observing mode."))
      for
        f  <- required(filter, "filter")
        u  <- required(fpu, "fpu")
        c  <- required(camera, "camera")
        g  <- required(grating.toOption, "grating")
        p  <- required(prism.toOption, "prism")
        w  <- required(centralWavelength, "centralWavelength")
      yield Create(exposureTimeMode, coadds, f, u, c, g, p,
                   w, explicitDecker.toOption,
                   explicitGrating.toOption, explicitPrism.toOption,
                   explicitFocusMotorSteps.toOption, explicitReadMode.toOption, explicitWellDepth.toOption,
                   explicitTelescopeConfigsSlit.toOption, telescopeConfigsIfu, acquisition,
                   telluricType.getOrElse(TelluricType.Hot))

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm),
          PosIntBinding.Option("coadds", rCoadds),
          GnirsFilterBinding.Option("filter", rFilter),
          GnirsSlitInput.Binding.Option("slit", rSlit),
          GnirsIfuInput.Binding.Option("ifu", rIfu),
          GnirsCameraBinding.Option("camera", rCamera),
          GnirsGratingBinding.Nullable("grating", rGrating),
          GnirsPrismBinding.Nullable("prism", rPrism),
          WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
          GnirsDeckerBinding.Nullable("explicitDecker", rDecker),
          GnirsGratingBinding.Nullable("explicitGrating", rExplGrating),
          GnirsPrismBinding.Nullable("explicitPrism", rExplPrism),
          IntBinding.Nullable("explicitFocusMotorSteps", rFocus),
          GnirsReadModeBinding.Nullable("explicitReadMode", rReadMode),
          GnirsWellDepthBinding.Nullable("explicitWellDepth", rWellDepth),
          AcquisitionInput.Binding.Option("acquisition", rAcq),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rEtm, rCoadds, rFilter, rSlit, rIfu, rCamera, rGrating, rPrism,
           rCentralWavelength, rDecker, rExplGrating, rExplPrism,
           rFocus, rReadMode, rWellDepth, rAcq, rTelluricType).parTupled.flatMap:
            (etm, coadds, filter, slit, ifu, camera, grating, prism,
             centralWavelength, decker, explGrating, explPrism,
             focus, readMode, wellDepth, acq, telluricType) =>
              resolveEdit(slit, ifu).map: (fpu, explTelescopeSlit, telescopeIfu) =>
                Edit(etm, coaddsForEtm(etm, coadds), filter, fpu, camera, grating, prism,
                     centralWavelength, decker, explGrating, explPrism,
                     focus, readMode, wellDepth, explTelescopeSlit, telescopeIfu, acq, telluricType)
