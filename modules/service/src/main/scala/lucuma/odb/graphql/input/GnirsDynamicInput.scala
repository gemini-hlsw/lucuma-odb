// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import coulomb.syntax.*
import grackle.Result
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStep
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.odb.graphql.binding.*

object GnirsAcquisitionMirrorOutInput:

  val Binding: Matcher[GnirsAcquisitionMirrorMode.Out] =
    ObjectFieldsBinding.rmap:
      case List(
        GnirsPrismBinding("prism", rPrism),
        GnirsGratingBinding("grating", rGrating),
        WavelengthInput.Binding("wavelength", rWavelength)
      ) => (rPrism, rGrating, rWavelength).parMapN: (prism, grating, wavelength) =>
        GnirsAcquisitionMirrorMode.Out(prism, grating, GnirsGratingWavelength(wavelength))

object GnirsDynamicInput:

  val Binding: Matcher[GnirsDynamicConfig] =
    ObjectFieldsBinding.rmap:
      case List(
        TimeSpanInput.Binding("exposure", rExposure),
        PosIntBinding("coadds", rCoadds),
        GnirsFilterBinding("filter", rFilter),
        GnirsDeckerBinding("decker", rDecker),
        GnirsFpuSlitBinding.Option("fpuSlit", rFpuSlit),
        GnirsFpuOtherBinding.Option("fpuOther", rFpuOther),
        GnirsFpuIfuBinding.Option("fpuIfu", rFpuIfu),
        GnirsAcquisitionMirrorOutInput.Binding.Option("acquisitionMirrorOut", rAcqMirror),
        GnirsCameraBinding("camera", rCamera),
        IntBinding.Option("focusMotorSteps", rFocusMotorSteps),
        GnirsReadModeBinding("readMode", rReadMode)
      ) => (rExposure, rCoadds, rFilter, rDecker, rFpuSlit, rFpuOther, rFpuIfu, rAcqMirror, rCamera, rFocusMotorSteps, rReadMode).parTupled.flatMap {
        case (exposure, coadds, filter, decker, fpuSlit, fpuOther, fpuIfu, acqMirror, camera, focusMotorSteps, readMode) =>

          val rFpu: Result[GnirsFpu] =
            (fpuSlit, fpuOther, fpuIfu) match
              case (Some(s), None,    None)    => Result.success(GnirsFpu.Spectroscopy.Slit(s))
              case (None,    Some(o), None)    => Result.success(GnirsFpu.Other(o))
              case (None,    None,    Some(i)) => Result.success(GnirsFpu.Spectroscopy.Ifu(i))
              case (None,    None,    None)    => Matcher.validationFailure("Exactly one of 'fpuSlit', 'fpuOther' or 'fpuIfu' must be provided.")
              case _                           => Matcher.validationFailure("Only one of 'fpuSlit', 'fpuOther' or 'fpuIfu' may be provided.")

          val rFocus: Result[GnirsFocus] =
            focusMotorSteps match
              case None    => Result.success(GnirsFocus.Best)
              case Some(i) =>
                GnirsFocusMotorStepsValue.from(i) match
                  case Right(v) => Result.success(GnirsFocus.Custom(v.withUnit[GnirsFocusMotorStep]))
                  case Left(m)  => Matcher.validationFailure(s"Invalid 'focusMotorSteps' value: $m")

          (rFpu, rFocus).parMapN: (fpu, focus) =>
            GnirsDynamicConfig(
              exposure,
              coadds,
              filter,
              decker,
              fpu,
              acqMirror.getOrElse(GnirsAcquisitionMirrorMode.In),
              camera,
              focus,
              readMode
            )
      }
