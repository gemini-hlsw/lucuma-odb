// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.apply.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsObsReadMode
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Access
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object GnirsLongSlitInput:

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
    filter:           Option[GnirsFilter],
    acqType:          Option[GnirsAcquisitionType],
    coadds:           Option[PosInt],
    offset:           Option[Offset],
    exposureTimeMode: Option[ExposureTimeMode]
  )

  object AcquisitionInput:
    val Binding: Matcher[AcquisitionInput] =
      ObjectFieldsBinding.rmap:
        case List(
          GnirsFilterBinding.Option("filter", rFilter),
          GnirsAcquisitionTypeBinding.Option("acqType", rAcqType),
          PosIntBinding.Option("coadds", rCoadds),
          OffsetInput.Binding.Option("offset", rOffset),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
        ) =>
          (rFilter, rAcqType, rCoadds, rOffset, rEtm).parMapN(AcquisitionInput.apply)

  case class Create(
    exposureTimeMode: Option[ExposureTimeMode],
    coadds:           Option[PosInt],
    filter:           GnirsFilter,
    fpu:              GnirsFpuSlit,
    camera:           GnirsCamera,
    grating:          GnirsGrating,
    prism:            GnirsPrism,
    explicitDecker:               Option[GnirsDecker]              = None,
    explicitGratingWavelength:    Option[Wavelength]               = None,
    explicitGrating:              Option[GnirsGrating]             = None,
    explicitPrism:                Option[GnirsPrism]               = None,
    explicitFocusMotorSteps:      Option[Int]                      = None,
    explicitReadMode:             Option[GnirsObsReadMode]         = None,
    explicitWellDepth:            Option[GnirsWellDepth]           = None,
    explicitTelescopeConfigs:     Option[SlitTelescopeConfigs]     = None,
    acquisition:                  Option[AcquisitionInput]         = None
  ):
    def observingModeType: ObservingModeType = ObservingModeType.GnirsLongSlit

    /** True if the input modifies fields that only Staff (or higher) may set. */
    def needsStaffAccess: Boolean = explicitFocusMotorSteps.isDefined

  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm),
          PosIntBinding.Option("coadds", rCoadds),
          GnirsFilterBinding("filter", rFilter),
          GnirsFpuSlitBinding("fpu", rFpu),
          GnirsCameraBinding("camera", rCamera),
          GnirsGratingBinding("grating", rGrating),
          GnirsPrismBinding("prism", rPrism),
          GnirsDeckerBinding.Option("explicitDecker", rDecker),
          WavelengthInput.Binding.Option("explicitGratingWavelength", rGratingWavelength),
          GnirsGratingBinding.Option("explicitGrating", rExplGrating),
          GnirsPrismBinding.Option("explicitPrism", rExplPrism),
          IntBinding.Option("explicitFocus", rFocus),
          GnirsObsReadModeBinding.Option("explicitReadMode", rReadMode),
          GnirsWellDepthBinding.Option("explicitWellDepth", rWellDepth),
          SlitTelescopeConfigsInput.Binding.Option("explicitTelescopeConfigs", rExplTelescope),
          AcquisitionInput.Binding.Option("acquisition", rAcq)
        ) =>
          (rEtm, rCoadds, rFilter, rFpu, rCamera, rGrating, rPrism,
           rDecker, rGratingWavelength, rExplGrating, rExplPrism,
           rFocus, rReadMode, rWellDepth, rExplTelescope, rAcq).parMapN:
            (etm, coadds, filter, fpu, camera, grating, prism,
             decker, gratingWavelength, explGrating, explPrism,
             focus, readMode, wellDepth, explTelescope, acq) =>
              Create(etm, coadds, filter, fpu, camera, grating, prism,
                     decker, gratingWavelength, explGrating, explPrism,
                     focus, readMode, wellDepth, explTelescope, acq)

  case class Edit(
    exposureTimeMode:          Option[ExposureTimeMode],
    coadds:                    Option[PosInt],          // Option, not Nullable — coadds is NOT NULL
    filter:                    Option[GnirsFilter],
    fpu:                       Option[GnirsFpuSlit],
    camera:                    Option[GnirsCamera],
    grating:                   Nullable[GnirsGrating],
    prism:                     Nullable[GnirsPrism],
    explicitDecker:            Nullable[GnirsDecker],
    explicitGratingWavelength: Nullable[Wavelength],
    explicitGrating:           Nullable[GnirsGrating],
    explicitPrism:             Nullable[GnirsPrism],
    explicitFocusMotorSteps:   Nullable[Int],
    explicitReadMode:          Nullable[GnirsObsReadMode],
    explicitWellDepth:         Nullable[GnirsWellDepth],
    explicitTelescopeConfigs:  Nullable[SlitTelescopeConfigs], // Nullable to allow clearing to default
    acquisition:               Option[AcquisitionInput]
  ):
    def observingModeType: ObservingModeType = ObservingModeType.GnirsLongSlit
    def updatesAcquisition: Boolean = acquisition.isDefined
    def limitToPreExecution(access: Access): Boolean = false

    /** True if the input modifies fields that only Staff (or higher) may set. */
    def needsStaffAccess: Boolean = explicitFocusMotorSteps.isDefined
    def toCreate: Result[Create] =
      def required[A](oa: Option[A], name: String): Result[A] =
        Result.fromOption(oa, Matcher.validationProblem(s"A $name is required to create a GNIRS Long Slit observing mode."))
      for
        f  <- required(filter, "filter")
        u  <- required(fpu, "fpu")
        c  <- required(camera, "camera")
        g  <- required(grating.toOption, "grating")
        p  <- required(prism.toOption, "prism")
      yield Create(exposureTimeMode, coadds, f, u, c, g, p,
                   explicitDecker.toOption, explicitGratingWavelength.toOption,
                   explicitGrating.toOption, explicitPrism.toOption,
                   explicitFocusMotorSteps.toOption, explicitReadMode.toOption, explicitWellDepth.toOption,
                   explicitTelescopeConfigs.toOption, acquisition)

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm),
          PosIntBinding.Option("coadds", rCoadds),
          GnirsFilterBinding.Option("filter", rFilter),
          GnirsFpuSlitBinding.Option("fpu", rFpu),
          GnirsCameraBinding.Option("camera", rCamera),
          GnirsGratingBinding.Nullable("grating", rGrating),
          GnirsPrismBinding.Nullable("prism", rPrism),
          GnirsDeckerBinding.Nullable("explicitDecker", rDecker),
          WavelengthInput.Binding.Nullable("explicitGratingWavelength", rGratingWavelength),
          GnirsGratingBinding.Nullable("explicitGrating", rExplGrating),
          GnirsPrismBinding.Nullable("explicitPrism", rExplPrism),
          IntBinding.Nullable("explicitFocus", rFocus),
          GnirsObsReadModeBinding.Nullable("explicitReadMode", rReadMode),
          GnirsWellDepthBinding.Nullable("explicitWellDepth", rWellDepth),
          SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rExplTelescope),
          AcquisitionInput.Binding.Option("acquisition", rAcq)
        ) =>
          (rEtm, rCoadds, rFilter, rFpu, rCamera, rGrating, rPrism,
           rDecker, rGratingWavelength, rExplGrating, rExplPrism,
           rFocus, rReadMode, rWellDepth, rExplTelescope, rAcq).parMapN(Edit.apply)
