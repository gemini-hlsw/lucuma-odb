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

sealed trait ProposalClassInput {
  def tag: Tag
  def minPercentTime: Option[IntPercent]
  def minPercentTotalTime: Option[IntPercent]
  def totalTime: Option[NonNegDuration]
}
object ProposalClassInput {

  case class TypeA(
    tag: Tag,
    minPercentTime: Option[IntPercent]
  ) extends ProposalClassInput {
    def minPercentTotalTime: Option[IntPercent] = None
    def totalTime: Option[NonNegDuration] = None
  }

  object TypeA {

    private def binding(tagValue: String): Matcher[TypeA] =
      ObjectFieldsBinding.rmap {
        case List(IntPercentBinding.Option("minPercentTime", rMinPercentTime)) =>
          rMinPercentTime.map(TypeA(Tag(tagValue), _))
      }

    def createBinding(tagValue: String): Matcher[ProposalClassInput] =
      binding(tagValue).rmap {
        case TypeA(_, None) => Result.failure(s"minPercentTime is required on creation")
        case ok             => Result(ok)
      }

    def editBinding(tagValue: String): Matcher[ProposalClassInput] =
      binding(tagValue)

  }

  case class TypeB(
    tag: Tag,
    minPercentTime: Option[IntPercent],
    minPercentTotalTime: Option[IntPercent],
    totalTime: Option[NonNegDuration],
  ) extends ProposalClassInput

  object TypeB {

    private def binding(tagValue: String): Matcher[ProposalClassInput] =
      ObjectFieldsBinding.rmap {
        case List(
          IntPercentBinding.Option("minPercentTime", rMinPercentTime),
          IntPercentBinding.Option("minPercentTotalTime", rMinPercentTotalTime),
          NonNegDurationInput.Binding.Option("totalTime", rTotalTime),
        ) =>
          (rMinPercentTime, rMinPercentTotalTime, rTotalTime).parMapN { (a, b, c) =>
            TypeB(Tag(tagValue), a, b, c)
          }
      }

    def createBinding(tagValue: String): Matcher[ProposalClassInput] =
      binding(tagValue).rmap {
        case ok @ TypeB(_, Some(_), Some(_), Some(_)) => Result(ok)
        case _ => Result.failure(s"All of minPercentTime, minPercentTotalTime, and totalTime are required on creation.")
      }

    def editBinding(tagValue: String): Matcher[ProposalClassInput] =
      binding(tagValue)

  }

  val CreateBinding: Matcher[ProposalClassInput] =
    binding(
      TypeA.createBinding("classical"),
      TypeA.createBinding("demo_science"),
      TypeA.createBinding("directors_time"),
      TypeA.createBinding("exchange"),
      TypeA.createBinding("fast_turnaround"),
      TypeA.createBinding("poor_weather"),
      TypeA.createBinding("queue"),
      TypeA.createBinding("system_verification"),
      TypeB.createBinding("intensive"),
      TypeB.createBinding("large_program"),
    )

  val EditBinding: Matcher[ProposalClassInput] =
    binding(
      TypeA.editBinding("classical"),
      TypeA.editBinding("demo_science"),
      TypeA.editBinding("directors_time"),
      TypeA.editBinding("exchange"),
      TypeA.editBinding("fast_turnaround"),
      TypeA.editBinding("poor_weather"),
      TypeA.editBinding("queue"),
      TypeA.editBinding("system_verification"),
      TypeB.editBinding("intensive"),
      TypeB.editBinding("large_program"),
    )

  private def binding(
    classical:          Matcher[ProposalClassInput],
    demoScience:        Matcher[ProposalClassInput],
    directorsTime:      Matcher[ProposalClassInput],
    exchange:           Matcher[ProposalClassInput],
    fastTurnaround:     Matcher[ProposalClassInput],
    poorWeather:        Matcher[ProposalClassInput],
    queue:              Matcher[ProposalClassInput],
    systemVerification: Matcher[ProposalClassInput],
    largeProgram:       Matcher[ProposalClassInput],
    intensive:          Matcher[ProposalClassInput],
  ): Matcher[ProposalClassInput] =
    ObjectFieldsBinding.rmap {
      case List(
        classical.Option("classical", rClassical),
        demoScience.Option("demoScience", rDemoScience),
        directorsTime.Option("directorsTime", rDirectorsTime),
        exchange.Option("exchange", rExchange),
        fastTurnaround.Option("fastTurnaround", rFastTurnaround),
        poorWeather.Option("poorWeather", rPoorWeather),
        queue.Option("queue", rQueue),
        systemVerification.Option("systemVerification", rSystemVerification),
        largeProgram.Option("largeProgram", rLargeProgram),
        intensive.Option("intensive", rIntensive),
      ) =>
        (rClassical, rDemoScience, rDirectorsTime, rExchange, rFastTurnaround, rIntensive, rLargeProgram, rPoorWeather, rQueue, rSystemVerification).parTupled.flatMap {
          (classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification) =>
            List(classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification).flatten match {
              case h :: Nil => Result(h)
              case other    => Result.failure(s"Expected exactly one of classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification; found ${other.length}.")
            }
        }
    }

}