// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.SignificantFigures
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class SpectroscopyGraphsInput(
  atWavelength:       Wavelength,
  exposureTime:       TimeSpan,
  exposureCount:      PosInt,
  asterism:           List[TargetDataInput],
  constraints:        ItcConstraintsInput,
  mode:               InstrumentModesInput,
  significantFigures: Option[SignificantFigures]
)

object SpectroscopyGraphsInput:

  val Binding: Matcher[SpectroscopyGraphsInput] =
    ObjectFieldsBinding.rmap:
      case List(
            WavelengthInput.Binding("atWavelength", wavelength),
            TimeSpanInput.Binding("exposureTime", exposureTime),
            PosIntBinding("exposureCount", exposureCount),
            TargetDataInput.Binding.List("asterism", asterism),
            ItcConstraintsInput.Binding("constraints", constraints),
            InstrumentModesInput.Binding("mode", mode),
            SignificantFiguresInput.Binding.Option("significantFigures", significantFigures)
          ) =>
        (wavelength, exposureTime, exposureCount, asterism, constraints, mode, significantFigures)
          .parMapN(apply)
