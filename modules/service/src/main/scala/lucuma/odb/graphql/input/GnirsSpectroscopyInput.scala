// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.apply.*
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
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Access
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
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
      case _: GnirsFpu.Spectroscopy.Slit => ObservingModeType.GnirsLongSlit
      case _: GnirsFpu.Spectroscopy.Ifu  => ObservingModeType.GnirsIfu

  // Exactly one of slitWidth (fpuSlit) / ifu (fpuIfu) is required on create.
  private def fpuFromSlitIfu(
    fpuSlit: Option[GnirsFpuSlit],
    fpuIfu:  Option[GnirsFpuIfu]
  ): Result[GnirsFpu.Spectroscopy] =
    (fpuSlit, fpuIfu) match
      case (Some(s), None) => Result(GnirsFpu.Spectroscopy.Slit(s))
      case (None, Some(i)) => Result(GnirsFpu.Spectroscopy.Ifu(i))
      case (None, None)    => Matcher.validationFailure("Exactly one of 'fpuSlit' or 'fpuIfu' must be provided.")
      case _               => Matcher.validationFailure("Only one of 'fpuSlit' or 'fpuIfu' may be provided.")

  object TelescopeConfigAlongSlitInput:
    val Binding: Matcher[TelescopeConfigAlongSlit] =
      ObjectFieldsBinding.rmap:
        case List(
          OffsetComponentInput.BindingQ("q", rQ),
          StepGuideStateBinding("guiding", rGuiding)
        ) =>
          (rQ, rGuiding).parMapN(TelescopeConfigAlongSlit.apply)

  object SlitTelescopeConfigsInput:
    val Binding: Matcher[SlitTelescopeConfigs] =
      ObjectFieldsBinding.rmap:
        case List(
          TelescopeConfigAlongSlitInput.Binding.List.Option("alongSlit", rAlongSlit),
          TelescopeConfigInput.Binding.List.Option("toSky", rOnSky)
        ) =>
          (rAlongSlit, rOnSky).tupled.flatMap:
            case (Some(cs), None) =>
              NonEmptyList.fromList(cs).fold(
                Matcher.validationFailure("alongSlit must not be empty")
              )(nel => Result(SlitTelescopeConfigs.AlongSlit(nel)))
            case (None, Some(cs)) =>
              NonEmptyList.fromList(cs).fold(
                Matcher.validationFailure("toSky must not be empty")
              )(nel => Result(SlitTelescopeConfigs.ToSky(nel)))
            case _ =>
              Matcher.validationFailure("Exactly one of alongSlit or toSky must be provided")

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
    explicitTelescopeConfigs:     Option[SlitTelescopeConfigs]     = None,
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
          GnirsFpuSlitBinding.Option("fpuSlit", rFpuSlit),
          GnirsFpuIfuBinding.Option("fpuIfu", rFpuIfu),
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
          SlitTelescopeConfigsInput.Binding.Option("explicitTelescopeConfigs", rExplTelescope),
          AcquisitionInput.Binding.Option("acquisition", rAcq),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rEtm, rCoadds, rFilter, rFpuSlit, rFpuIfu, rCamera, rGrating, rPrism,
           rCentralWavelength, rDecker, rExplGrating, rExplPrism,
           rFocus, rReadMode, rWellDepth, rExplTelescope, rAcq, rTelluricType).parTupled.flatMap:
            (etm, coadds, filter, fpuSlit, fpuIfu, camera, grating, prism,
             centralWavelength, decker, explGrating, explPrism,
             focus, readMode, wellDepth, explTelescope, acq, telluricType) =>
              fpuFromSlitIfu(fpuSlit, fpuIfu).map: fpu =>
                Create(etm, coaddsForEtm(etm, coadds), filter, fpu, camera, grating, prism,
                       centralWavelength, decker, explGrating, explPrism,
                       focus, readMode, wellDepth, explTelescope, acq,
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
    explicitTelescopeConfigs:  Nullable[SlitTelescopeConfigs], // Nullable to allow clearing to default
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
                   explicitTelescopeConfigs.toOption, acquisition,
                   telluricType.getOrElse(TelluricType.Hot))

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm),
          PosIntBinding.Option("coadds", rCoadds),
          GnirsFilterBinding.Option("filter", rFilter),
          GnirsFpuSlitBinding.Option("fpuSlit", rFpuSlit),
          GnirsFpuIfuBinding.Option("fpuIfu", rFpuIfu),
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
          SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rExplTelescope),
          AcquisitionInput.Binding.Option("acquisition", rAcq),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rEtm, rCoadds, rFilter, rFpuSlit, rFpuIfu, rCamera, rGrating, rPrism,
           rCentralWavelength, rDecker, rExplGrating, rExplPrism,
           rFocus, rReadMode, rWellDepth, rExplTelescope, rAcq, rTelluricType).parTupled.flatMap:
            (etm, coadds, filter, fpuSlit, fpuIfu, camera, grating, prism,
             centralWavelength, decker, explGrating, explPrism,
             focus, readMode, wellDepth, explTelescope, acq, telluricType) =>
              // At most one of fpuSlit / fpuIfu may be edited at a time.
              val rFpu: Result[Option[GnirsFpu.Spectroscopy]] =
                (fpuSlit, fpuIfu) match
                  case (None, None)    => Result(none)
                  case (Some(s), None) => Result(GnirsFpu.Spectroscopy.Slit(s).some)
                  case (None, Some(i)) => Result(GnirsFpu.Spectroscopy.Ifu(i).some)
                  case _               => Matcher.validationFailure("Only one of 'fpuSlit' or 'fpuIfu' may be provided.")
              rFpu.map: fpu =>
                Edit(etm, coaddsForEtm(etm, coadds), filter, fpu, camera, grating, prism,
                     centralWavelength, decker, explGrating, explPrism,
                     focus, readMode, wellDepth, explTelescope, acq, telluricType)
