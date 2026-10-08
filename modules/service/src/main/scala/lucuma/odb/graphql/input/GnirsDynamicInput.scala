// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
      ) =>
        val rFpu: Result[GnirsFpu] =
          (rFpuSlit, rFpuOther, rFpuIfu).parFlatMapN: (slit, other, ifu) =>
            oneOrFail[GnirsFpu](
              slit.map(GnirsFpu.Spectroscopy.Slit(_)) -> "fpuSlit",
              other.map(GnirsFpu.Other(_))            -> "fpuOther",
              ifu.map(GnirsFpu.Spectroscopy.Ifu(_))   -> "fpuIfu"
            )

        val rFocus: Result[GnirsFocus] =
          rFocusMotorSteps.flatMap:
            case None    => Result.success(GnirsFocus.Best)
            case Some(i) =>
              GnirsFocusMotorStepsValue.from(i) match
                case Right(v) => Result.success(GnirsFocus.Custom(v.withUnit[GnirsFocusMotorStep]))
                case Left(m)  => Result.failure(s"Invalid 'focusMotorSteps' value: $m")

        val rAcqMirrorʹ: Result[GnirsAcquisitionMirrorMode] =
          rAcqMirror.map(_.getOrElse(GnirsAcquisitionMirrorMode.In))

        (rExposure, rCoadds, rFilter, rDecker, rFpu, rAcqMirrorʹ, rCamera, rFocus, rReadMode).parMapN:
          GnirsDynamicConfig.apply
