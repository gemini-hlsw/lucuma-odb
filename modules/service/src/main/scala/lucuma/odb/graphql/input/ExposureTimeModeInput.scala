// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ExposureTimeMode.SignalToNoiseMode
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
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
          for
            t <- rTime
            _ <- OdbError.InvalidArgument("Exposure `time` parameter must be positive.".some).asFailure.unlessA(t.toNonNegMicroseconds.value > 0)
            c <- rCount
            a <- rAt
          yield TimeAndCountMode(t, c, a)

  val Binding: Matcher[ExposureTimeMode] =
    ObjectFieldsBinding.rmap:
      case List(
        SignalToNoise.Binding.Option("signalToNoise", rSignal),
        TimeAndCount.Binding.Option("timeAndCount", rTimeAndCount)
      ) =>
        (rSignal, rTimeAndCount).tupled.flatMap:
          case (None,    None   ) => OdbError.InvalidArgument("One of 'signalToNoise' or 'timeAndCount' must be selected.".some).asFailure
          case (Some(s), None   ) => Result(ExposureTimeMode.signalToNoise.reverseGet(s))
          case (None,    Some(f)) => Result(ExposureTimeMode.timeAndCount.reverseGet(f))
          case _                  => OdbError.InvalidArgument("Exactly one of 'signalToNoise' or 'timeAndCount' must be selected, not both.".some).asFailure