// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Order.given
import cats.data.NonEmptyList
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import grackle.Result
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
import lucuma.core.model.Access
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object GnirsSpectroscopyInput:

  /**
   * Validates a central wavelength list: at least one entry, no duplicated
   * wavelength (each one backs a distinct row in t_gnirs_spectroscopy_wavelength),
   * returned sorted by increasing wavelength, which is the order the sequence
   * executes them in.
   */
  private def resolveWavelengths(
    ws: List[GnirsSpectroscopyWavelengthInput]
  ): Result[NonEmptyList[GnirsSpectroscopyWavelengthInput]] =
    val duplicates = ws.groupBy(_.centralWavelength).filter(_._2.sizeIs > 1).keys.toList
    if duplicates.nonEmpty then
      Matcher.validationFailure:
        s"Duplicate central wavelengths are not allowed: ${duplicates.sorted.map(_.toNanometers.value.value).mkString(", ")} nm."
    else
      Result.fromOption(
        NonEmptyList.fromList(ws.sortBy(_.centralWavelength)),
        Matcher.validationProblem("At least one central wavelength must be specified for GNIRS spectroscopy observations.")
      )

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

  // The acquisition customization input is shared with the other GNIRS modes.
  type AcquisitionInput = GnirsAcquisitionInput
  val AcquisitionInput = GnirsAcquisitionInput

  case class Create(
    centralWavelengths: NonEmptyList[GnirsSpectroscopyWavelengthInput],
    filter:           GnirsFilter,
    fpu:              GnirsFpu.Spectroscopy,
    camera:           GnirsCamera,
    grating:          GnirsGrating,
    prism:            GnirsPrism,
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
          GnirsSpectroscopyWavelengthInput.Binding.List.Option("centralWavelengths", rCentralWavelengths),
          GnirsFilterBinding("filter", rFilter),
          GnirsSlitInput.Binding.Option("slit", rSlit),
          GnirsIfuInput.Binding.Option("ifu", rIfu),
          GnirsCameraBinding("camera", rCamera),
          GnirsGratingBinding("grating", rGrating),
          GnirsPrismBinding("prism", rPrism),
          GnirsDeckerBinding.Option("explicitDecker", rDecker),
          GnirsGratingBinding.Option("explicitGrating", rExplGrating),
          GnirsPrismBinding.Option("explicitPrism", rExplPrism),
          IntBinding.Option("explicitFocusMotorSteps", rFocus),
          GnirsReadModeBinding.Option("explicitReadMode", rReadMode),
          GnirsWellDepthBinding.Option("explicitWellDepth", rWellDepth),
          AcquisitionInput.Binding.Option("acquisition", rAcq),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rCentralWavelengths, rFilter, rSlit, rIfu, rCamera, rGrating, rPrism,
           rDecker, rExplGrating, rExplPrism,
           rFocus, rReadMode, rWellDepth, rAcq, rTelluricType).parTupled.flatMap:
            (centralWavelengths, filter, slit, ifu, camera, grating, prism,
             decker, explGrating, explPrism,
             focus, readMode, wellDepth, acq, telluricType) =>
              (resolveWavelengths(centralWavelengths.getOrElse(Nil)),
               resolveCreate(slit, ifu)
              ).parMapN: (ws, resolved) =>
                val (fpu, explTelescopeSlit, telescopeIfu) = resolved
                Create(ws, filter, fpu, camera, grating, prism,
                       decker, explGrating, explPrism,
                       focus, readMode, wellDepth, explTelescopeSlit, telescopeIfu, acq,
                       telluricType.getOrElse(TelluricType.Hot))

  case class Edit(
    centralWavelengths:        Option[NonEmptyList[GnirsSpectroscopyWavelengthInput]],
    filter:                    Option[GnirsFilter],
    fpu:                       Option[GnirsFpu.Spectroscopy],
    camera:                    Option[GnirsCamera],
    grating:                   Nullable[GnirsGrating],
    prism:                     Nullable[GnirsPrism],
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
        ws <- required(centralWavelengths, "centralWavelengths")
      yield Create(ws, f, u, c, g, p,
                   explicitDecker.toOption,
                   explicitGrating.toOption, explicitPrism.toOption,
                   explicitFocusMotorSteps.toOption, explicitReadMode.toOption, explicitWellDepth.toOption,
                   explicitTelescopeConfigsSlit.toOption, telescopeConfigsIfu, acquisition,
                   telluricType.getOrElse(TelluricType.Hot))

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          GnirsSpectroscopyWavelengthInput.Binding.List.Option("centralWavelengths", rCentralWavelengths),
          GnirsFilterBinding.Option("filter", rFilter),
          GnirsSlitInput.Binding.Option("slit", rSlit),
          GnirsIfuInput.Binding.Option("ifu", rIfu),
          GnirsCameraBinding.Option("camera", rCamera),
          GnirsGratingBinding.Nullable("grating", rGrating),
          GnirsPrismBinding.Nullable("prism", rPrism),
          GnirsDeckerBinding.Nullable("explicitDecker", rDecker),
          GnirsGratingBinding.Nullable("explicitGrating", rExplGrating),
          GnirsPrismBinding.Nullable("explicitPrism", rExplPrism),
          IntBinding.Nullable("explicitFocusMotorSteps", rFocus),
          GnirsReadModeBinding.Nullable("explicitReadMode", rReadMode),
          GnirsWellDepthBinding.Nullable("explicitWellDepth", rWellDepth),
          AcquisitionInput.Binding.Option("acquisition", rAcq),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rCentralWavelengths, rFilter, rSlit, rIfu, rCamera, rGrating, rPrism,
           rDecker, rExplGrating, rExplPrism,
           rFocus, rReadMode, rWellDepth, rAcq, rTelluricType).parTupled.flatMap:
            (centralWavelengths, filter, slit, ifu, camera, grating, prism,
             decker, explGrating, explPrism,
             focus, readMode, wellDepth, acq, telluricType) =>
              (centralWavelengths.traverse(resolveWavelengths),
               resolveEdit(slit, ifu)
              ).parMapN: (ws, resolved) =>
                val (fpu, explTelescopeSlit, telescopeIfu) = resolved
                Edit(ws, filter, fpu, camera, grating, prism,
                     decker, explGrating, explPrism,
                     focus, readMode, wellDepth, explTelescopeSlit, telescopeIfu, acq, telluricType)
