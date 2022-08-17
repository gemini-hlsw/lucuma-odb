// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._
import lucuma.odb.data.Tag
import lucuma.core.enums.ToOActivation
import lucuma.core.model.NonNegDuration
import edu.gemini.grackle.Result
import lucuma.core.model.IntPercent

case class ProposalClassInput(
  tag: Tag,
  minPercentTime: Option[IntPercent],
  minPercentTotalTime: Option[IntPercent],
  totalTime: Option[NonNegDuration],
)


object ProposalClassInput {

  private val ClassicalBinding          = small("Classical")
  private val DemoScienceBinding        = small("DemoScience")
  private val DirectorsTimeBinding      = small("DirectorsTime")
  private val ExchangeBinding           = small("Exchange")
  private val FastTurnaroundBinding     = small("FastTurnaround")
  private val IntensiveBinding          = large("Intensive")
  private val LargeProgramBinding       = large("LargeProgram")
  private val PoorWeatherBinding        = small("PoorWeather")
  private val QueueBinding              = small("Queue")
  private val SystemVerificationBinding = small("SystemVerification")

  val Binding: Matcher[ProposalClassInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ClassicalBinding.Option("classical", rClassical),
        DemoScienceBinding.Option("demoScience", rDemoScience),
        DirectorsTimeBinding.Option("directorsTime", rDirectorsTime),
        ExchangeBinding.Option("exchange", rExchange),
        FastTurnaroundBinding.Option("fastTurnaround", rFastTurnaround),
        PoorWeatherBinding.Option("poorWeather", rPoorWeather),
        QueueBinding.Option("queue", rQueue),
        SystemVerificationBinding.Option("systemVerification", rSystemVerification),
        LargeProgramBinding.Option("largeProgram", rLargeProgram),
        IntensiveBinding.Option("intensive", rIntensive),
      ) =>
        (rClassical, rDemoScience, rDirectorsTime, rExchange, rFastTurnaround, rIntensive, rLargeProgram, rPoorWeather, rQueue, rSystemVerification).parTupled.flatMap {
          (classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification) =>
            List(classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification).flatten match {
              case h :: Nil => Result(h)
              case other    => Result.failure(s"Expected exactly one of classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification; found ${other.length}.")
            }
        }
    }

  private def small(tagValue: String): Matcher[ProposalClassInput] =
    ObjectFieldsBinding.rmap {
      case List(
        IntPercentBinding.Option("minPercentTime", rMinPercentTime)
      ) =>
        rMinPercentTime.map(apply(Tag(tagValue), _, None, None))
    }

  private def large(tagValue: String): Matcher[ProposalClassInput] =
    ObjectFieldsBinding.rmap {
      case List(
        IntPercentBinding.Option("minPercentTime", rMinPercentTime),
        IntPercentBinding.Option("minPercentTotalTime", rMinPercentTotalTime),
        NonNegDurationInput.Binding.Option("totalTime", rTotalTime),
      ) =>
        (rMinPercentTime, rMinPercentTotalTime, rTotalTime).parMapN(apply(Tag(tagValue), _, _, _))
    }

}