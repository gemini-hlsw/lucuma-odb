// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import grackle.Result
import lucuma.core.model.IntPercent
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding._

object ProposalClassInput {

  object TypeA {

    case class Create(
      tag: Tag,
      minPercentTime: IntPercent
    )

    case class Edit(
      tag: Tag,
      minPercentTime: Option[IntPercent]
    ) {
      def asCreate: Option[Create] = minPercentTime.map(Create(tag, _))
    }

    private val DataBinding: Matcher[Option[IntPercent]] =
      ObjectFieldsBinding.rmap {
        case List(IntPercentBinding.Option("minPercentTime", rMinPercentTime)) =>
          rMinPercentTime
      }

    def createBinding(tag: Tag): Matcher[Create] =
      DataBinding.rmap {
        case Some(t)  => Result(Create(tag, t))
        case None     => Matcher.validationFailure(s"minPercentTime is required on creation")
      }

    def editBinding(tag: Tag): Matcher[Edit] =
      DataBinding.map(Edit(tag, _))

  }

  object TypeB {

    case class Create(
      tag: Tag,
      minPercentTime: IntPercent,
      minPercentTotalTime: IntPercent,
      totalTime: TimeSpan,
    )

    case class Edit(
      tag: Tag,
      minPercentTime: Option[IntPercent],
      minPercentTotalTime: Option[IntPercent],
      totalTime: Option[TimeSpan]
     ) {
      def asCreate = (minPercentTime, minPercentTotalTime, totalTime).mapN(Create(tag, _, _, _))
    }

    private val DataBinding: Matcher[(Option[IntPercent], Option[IntPercent], Option[TimeSpan])] =
      ObjectFieldsBinding.rmap {
        case List(
          IntPercentBinding.Option("minPercentTime", rMinPercentTime),
          IntPercentBinding.Option("minPercentTotalTime", rMinPercentTotalTime),
          TimeSpanInput.Binding.Option("totalTime", rTotalTime),
        ) =>
          (rMinPercentTime, rMinPercentTotalTime, rTotalTime).parTupled
      }

    def createBinding(tag: Tag): Matcher[Create] =
      DataBinding.rmap { tup =>
        Result.fromOption(
          tup.mapN(Create(tag, _, _, _)),
          s"All of minPercentTime, minPercentTotalTime, and totalTime are required on creation."
        )
      }

    def editBinding(tag: Tag): Matcher[Edit] =
      DataBinding.map(Edit(tag, _, _, _))

  }

  private def binding[A, B](
    classical:          Matcher[A],
    demoScience:        Matcher[A],
    directorsTime:      Matcher[A],
    exchange:           Matcher[A],
    fastTurnaround:     Matcher[A],
    poorWeather:        Matcher[A],
    queue:              Matcher[A],
    systemVerification: Matcher[A],
    largeProgram:       Matcher[B],
    intensive:          Matcher[B],
  ): Matcher[Either[A, B]] =
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
        (rClassical, rDemoScience, rDirectorsTime, rExchange, rFastTurnaround, rPoorWeather, rQueue, rSystemVerification, rLargeProgram, rIntensive).parTupled.flatMap {
          (classical, demoScience, directorsTime, exchange, fastTurnaround, poorWeather, queue, systemVerification, largeProgram, intensive) =>
            List(
              classical.map(_.asLeft),
              demoScience.map(_.asLeft),
              directorsTime.map(_.asLeft),
              exchange.map(_.asLeft),
              fastTurnaround.map(_.asLeft),
              poorWeather.map(_.asLeft),
              queue.map(_.asLeft),
              systemVerification.map(_.asLeft),
              largeProgram.map(_.asRight),
              intensive.map(_.asRight),
            ).flatten match {
              case h :: Nil => Result(h)
              case other    => Matcher.validationFailure(s"Expected exactly one of classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification; found ${other.length}.")
            }
        }
    }

  val CreateBinding: Matcher[Either[TypeA.Create, TypeB.Create]] =
    binding(
      TypeA.createBinding(Tag("classical")),
      TypeA.createBinding(Tag("demo_science")),
      TypeA.createBinding(Tag("directors_time")),
      TypeA.createBinding(Tag("exchange")),
      TypeA.createBinding(Tag("fast_turnaround")),
      TypeA.createBinding(Tag("poor_weather")),
      TypeA.createBinding(Tag("queue")),
      TypeA.createBinding(Tag("system_verification")),
      TypeB.createBinding(Tag("large_program")),
      TypeB.createBinding(Tag("intensive")),
    )

  val EditBinding: Matcher[Either[TypeA.Edit, TypeB.Edit]] =
    binding(
      TypeA.editBinding(Tag("classical")),
      TypeA.editBinding(Tag("demo_science")),
      TypeA.editBinding(Tag("directors_time")),
      TypeA.editBinding(Tag("exchange")),
      TypeA.editBinding(Tag("fast_turnaround")),
      TypeA.editBinding(Tag("poor_weather")),
      TypeA.editBinding(Tag("queue")),
      TypeA.editBinding(Tag("system_verification")),
      TypeB.editBinding(Tag("large_program")),
      TypeB.editBinding(Tag("intensive")),
    )

}


// sealed trait ProposalClassInput {
//   def tag: Tag
//   def minPercentTime: Option[IntPercent]
//   def minPercentTotalTime: Option[IntPercent]
//   def totalTime: Option[NonNegDuration]
// }
// object ProposalClassInput {

//   case class TypeA(
//     tag: Tag,
//     minPercentTime: Option[IntPercent]
//   ) extends ProposalClassInput {
//     def minPercentTotalTime: Option[IntPercent] = None
//     def totalTime: Option[NonNegDuration] = None
//   }

//   object TypeA {

//     private def binding(tagValue: String): Matcher[TypeA] =
//       ObjectFieldsBinding.rmap {
//         case List(IntPercentBinding.Option("minPercentTime", rMinPercentTime)) =>
//           rMinPercentTime.map(TypeA(Tag(tagValue), _))
//       }

//     def createBinding(tagValue: String): Matcher[ProposalClassInput] =
//       binding(tagValue).rmap {
//         case TypeA(_, None) => Matcher.validationFailure(s"minPercentTime is required on creation")
//         case ok             => Result(ok)
//       }

//     def editBinding(tagValue: String): Matcher[ProposalClassInput] =
//       binding(tagValue)

//   }

//   case class TypeB(
//     tag: Tag,
//     minPercentTime: Option[IntPercent],
//     minPercentTotalTime: Option[IntPercent],
//     totalTime: Option[NonNegDuration],
//   ) extends ProposalClassInput

//   object TypeB {

//     private def binding(tagValue: String): Matcher[ProposalClassInput] =
//       ObjectFieldsBinding.rmap {
//         case List(
//           IntPercentBinding.Option("minPercentTime", rMinPercentTime),
//           IntPercentBinding.Option("minPercentTotalTime", rMinPercentTotalTime),
//           NonNegDurationInput.Binding.Option("totalTime", rTotalTime),
//         ) =>
//           (rMinPercentTime, rMinPercentTotalTime, rTotalTime).parMapN { (a, b, c) =>
//             TypeB(Tag(tagValue), a, b, c)
//           }
//       }

//     def createBinding(tagValue: String): Matcher[ProposalClassInput] =
//       binding(tagValue).rmap {
//         case ok @ TypeB(_, Some(_), Some(_), Some(_)) => Result(ok)
//         case _ => Matcher.validationFailure(s"All of minPercentTime, minPercentTotalTime, and totalTime are required on creation.")
//       }

//     def editBinding(tagValue: String): Matcher[ProposalClassInput] =
//       binding(tagValue)

//   }

//   val CreateBinding: Matcher[ProposalClassInput] =
//     binding(
//       TypeA.createBinding("classical"),
//       TypeA.createBinding("demo_science"),
//       TypeA.createBinding("directors_time"),
//       TypeA.createBinding("exchange"),
//       TypeA.createBinding("fast_turnaround"),
//       TypeA.createBinding("poor_weather"),
//       TypeA.createBinding("queue"),
//       TypeA.createBinding("system_verification"),
//       TypeB.createBinding("intensive"),
//       TypeB.createBinding("large_program"),
//     )

//   val EditBinding: Matcher[ProposalClassInput] =
//     binding(
//       TypeA.editBinding("classical"),
//       TypeA.editBinding("demo_science"),
//       TypeA.editBinding("directors_time"),
//       TypeA.editBinding("exchange"),
//       TypeA.editBinding("fast_turnaround"),
//       TypeA.editBinding("poor_weather"),
//       TypeA.editBinding("queue"),
//       TypeA.editBinding("system_verification"),
//       TypeB.editBinding("intensive"),
//       TypeB.editBinding("large_program"),
//     )

//   private def binding(
//     classical:          Matcher[ProposalClassInput],
//     demoScience:        Matcher[ProposalClassInput],
//     directorsTime:      Matcher[ProposalClassInput],
//     exchange:           Matcher[ProposalClassInput],
//     fastTurnaround:     Matcher[ProposalClassInput],
//     poorWeather:        Matcher[ProposalClassInput],
//     queue:              Matcher[ProposalClassInput],
//     systemVerification: Matcher[ProposalClassInput],
//     largeProgram:       Matcher[ProposalClassInput],
//     intensive:          Matcher[ProposalClassInput],
//   ): Matcher[ProposalClassInput] =
//     ObjectFieldsBinding.rmap {
//       case List(
//         classical.Option("classical", rClassical),
//         demoScience.Option("demoScience", rDemoScience),
//         directorsTime.Option("directorsTime", rDirectorsTime),
//         exchange.Option("exchange", rExchange),
//         fastTurnaround.Option("fastTurnaround", rFastTurnaround),
//         poorWeather.Option("poorWeather", rPoorWeather),
//         queue.Option("queue", rQueue),
//         systemVerification.Option("systemVerification", rSystemVerification),
//         largeProgram.Option("largeProgram", rLargeProgram),
//         intensive.Option("intensive", rIntensive),
//       ) =>
//         (rClassical, rDemoScience, rDirectorsTime, rExchange, rFastTurnaround, rIntensive, rLargeProgram, rPoorWeather, rQueue, rSystemVerification).parTupled.flatMap {
//           (classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification) =>
//             List(classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification).flatten match {
//               case h :: Nil => Result(h)
//               case other    => Matcher.validationFailure(s"Expected exactly one of classical, demoScience, directorsTime, exchange, fastTurnaround, intensive, largeProgram, poorWeather, queue, systemVerification; found ${other.length}.")
//             }
//         }
//     }

// }
