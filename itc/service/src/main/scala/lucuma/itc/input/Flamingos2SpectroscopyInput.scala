// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.PortDisposition
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.itc.binding.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ExposureTimeModeInput
import lucuma.odb.graphql.input.Flamingos2FpuMaskInput

case class Flamingos2SpectroscopyInput(
  exposureTimeMode: ExposureTimeMode,
  disperser:        Flamingos2Disperser,
  filter:           Flamingos2Filter,
  readMode:         Flamingos2ReadMode,
  fpu:              Flamingos2FpuMask,
  port:             PortDisposition
) extends InstrumentModesInput

object Flamingos2SpectroscopyInput:

  def binding: Matcher[Flamingos2SpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            Flamingos2DisperserBinding("disperser", disperser),
            Flamingos2FpuMaskInput.Binding("fpu", fpu),
            Flamingos2FilterBinding("filter", filter),
            Flamingos2ReadModeBinding("readMode", readMode),
            PortDispositionBinding("port", portDisposition)
          ) =>
        (exposureTimeMode, disperser, filter, readMode, fpu.flatMap(validateFpu), portDisposition)
          .parMapN(apply)
    }

  // The shared FPU binding defaults to imaging and accepts any custom slit width, but the
  // legacy ITC needs a slit with a defined width.
  private def validateFpu(fpu: Flamingos2FpuMask): Result[Flamingos2FpuMask] =
    fpu match
      case Flamingos2FpuMask.Imaging                                    =>
        Matcher.validationFailure("Flamingos 2 spectroscopy requires a focal plane unit.")
      case Flamingos2FpuMask.Custom(_, Flamingos2CustomSlitWidth.Other) =>
        Matcher.validationFailure(
          "Flamingos 2 custom slit width Other is not supported by the ITC, it has no defined width."
        )
      case other                                                        => Result(other)
