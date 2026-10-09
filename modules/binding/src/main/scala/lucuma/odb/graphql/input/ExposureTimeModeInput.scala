// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ExposureTimeMode.SignalToNoiseMode
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

object ExposureTimeModeInput:

  object SignalToNoise:
    val Binding: Matcher[SignalToNoiseMode] =
      ObjectFieldsBinding.rmap {
        case List(
          SignalToNoiseBinding("value", rValue),
          WavelengthInput.Binding("at", rAt)
        ) =>
          (rValue, rAt).parMapN(SignalToNoiseMode.apply)
      }
      
  object TimeAndCount:
    val Binding: Matcher[TimeAndCountMode] =
      ObjectFieldsBinding.rmap:
        case List(
          TimeSpanInput.Binding("time", rTime),
          PosIntBinding("count", rCount),
          WavelengthInput.Binding("at", rAt)
        ) =>
          val rTimeʹ = rTime.flatMap: t =>
            if t.toNonNegMicroseconds.value > 0 then t.success
            else Result.failure("Exposure `time` parameter must be positive.")
          (rTimeʹ, rCount, rAt).parMapN(TimeAndCountMode.apply)

  val Binding: Matcher[ExposureTimeMode] =
    ObjectFieldsBinding.rmap:
      case List(
        SignalToNoise.Binding.Option("signalToNoise", rSignal),
        TimeAndCount.Binding.Option("timeAndCount", rTimeAndCount)
      ) =>
        (rSignal, rTimeAndCount).parFlatMapN: (signal, timeAndCount) =>
          oneOrFail(
            signal.map(ExposureTimeMode.signalToNoise.reverseGet)      -> "signalToNoise",
            timeAndCount.map(ExposureTimeMode.timeAndCount.reverseGet) -> "timeAndCount"
          )
