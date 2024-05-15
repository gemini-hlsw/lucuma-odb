// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

object ProposalTypeInput {

  private val fieldNames: List[String] =
    List(
      "classical",
      "demoScience",
      "directorsTime",
      "fastTurnaround",
      "largeProgram",
      "poorWeather",
      "queue",
      "systemVerfication"
    )

  private lazy val formattedFieldNames: String = {
    val fs = fieldNames.map(n => s"'$n'")
    val prefix = fs.init.intercalate(", ")
    s"$prefix or ${fs.last}"
  }

  private lazy val ZeroPercent    = IntPercent.unsafeFrom(0)
  private lazy val HundredPercent = IntPercent.unsafeFrom(100)

  private val PartnerSplitsInput: Matcher[Map[Tag, IntPercent]] =
    PartnerSplitInput.Binding.List.Option.rmap { splits =>
      val map = splits.getOrElse(List.empty).map(a => (a.partner -> a.percent)).toMap

      Matcher
        .validationFailure("Each partner may only appear once.")
        .unlessA(splits.forall(_.length === map.size)) *>
      Matcher
        .validationFailure("Percentages must sum to exactly 100.")
        .unlessA(splits.forall(_.foldMap(_.percent.value) === 100)) *>
      map.success

    }

  case class Create(
    scienceSubtype:  ScienceSubtype,
    tooActivation:   ToOActivation,
    minPercentTime:  IntPercent,
    minPercentTotal: Option[IntPercent]   = None,
    totalTime:       Option[TimeSpan]     = None,
    partnerSplits:   Map[Tag, IntPercent] = Map.empty
  ) {
    def asEdit: Edit =
      Edit(
        scienceSubtype,
        tooActivation.some,
        minPercentTime.some,
        Nullable.orNull(minPercentTotal),
        Nullable.orNull(totalTime),
        partnerSplits.some
      )
  }

  object Create {

    val Default: Create =
      Create(ScienceSubtype.Queue, ToOActivation.None, ZeroPercent)

    private def simpleCreateBinding(s: ScienceSubtype): Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin)
        ) => (rToo, rMin).parMapN { (too, min) => Create(s, too, min) }
      }

    private val Classical: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          IntPercentBinding("minPercentTime", rMin),
          PartnerSplitsInput.Option("partnerSplits", rSplits)
        ) => (rMin, rSplits).parMapN { (min, splits) =>
          Create(ScienceSubtype.Classical, ToOActivation.None, min, partnerSplits = splits.getOrElse(Map.empty))
        }
      }

    private val DemoScience: Matcher[Create] =
      simpleCreateBinding(ScienceSubtype.DemoScience)

    private val DirectorsTime: Matcher[Create] =
      simpleCreateBinding(ScienceSubtype.DirectorsTime)

    private val FastTurnaround: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          TagBinding("piAffiliation", rPartner)
        ) => (rToo, rMin, rPartner).parMapN { (too, min, partner) =>
          Create(ScienceSubtype.FastTurnaround, too, min, partnerSplits = Map(partner -> HundredPercent))
        }
      }

    private val LargeProgram: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          IntPercentBinding("minPercentTotalTime", rMinTotal),
          TimeSpanInput.Binding("totalTime", rTotal)
        ) => (rToo, rMin, rMinTotal, rTotal).parMapN { (too, min, minTotal, total) =>
          Create(ScienceSubtype.LargeProgram, too, min, minTotal.some, total.some)
        }
      }

    private val PoorWeather: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          EnumBinding.Option("ignore", rIgnore)
        ) => rIgnore.as(Create(ScienceSubtype.PoorWeather, ToOActivation.None, ZeroPercent))
      }

    private val Queue: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          PartnerSplitsInput("partnerSplits", rSplits)
        ) => (rToo, rMin, rSplits).parMapN { (too, min, splits) =>
          Create(ScienceSubtype.Queue, too, min, partnerSplits = splits)
        }
      }

    private val SystemVerification: Matcher[Create] =
      simpleCreateBinding(ScienceSubtype.SystemVerification)

    val Binding: Matcher[Create] =
      binding(
        Classical,
        DemoScience,
        DirectorsTime,
        FastTurnaround,
        LargeProgram,
        PoorWeather,
        Queue,
        SystemVerification
      )
  }

  case class Edit(
    scienceSubtype:  ScienceSubtype,
    tooActivation:   Option[ToOActivation]        = None,
    minPercentTime:  Option[IntPercent]           = None,
    minPercentTotal: Nullable[IntPercent]         = Nullable.Null,
    totalTime:       Nullable[TimeSpan]           = Nullable.Null,
    partnerSplits:   Option[Map[Tag, IntPercent]] = None
  ) {


    def asCreate: Result[Create] = {
      def toResult[A](oa: Option[A], s:  => String)(f: A => Create): Result[Create] =
        Result.fromOption(oa, OdbError.InvalidArgument(s.some).asProblem).map(f)

      scienceSubtype match {
        case ScienceSubtype.Classical          =>
          toResult(
            (minPercentTime, partnerSplits).tupled,
            "'minPercentTime' and 'partnerSplits' are required for classical proposals."
          ) { case (min, splits) => Create(scienceSubtype, ToOActivation.None, min, partnerSplits = splits) }

        case ScienceSubtype.DemoScience   |
             ScienceSubtype.DirectorsTime |
             ScienceSubtype.SystemVerification =>
          toResult(
            (tooActivation, minPercentTime).tupled,
            s"'toOActivation' and 'minPercentTime' are required for ${scienceSubtype.tag.replace('_', ' ')} proposals."
          ) { case (too, min) => Create(scienceSubtype, too, min) }

        case ScienceSubtype.FastTurnaround     =>
          toResult(
            (tooActivation, minPercentTime, partnerSplits).tupled,
            "'toOActivation', 'minPercentTime' and 'piAffiliate' are required for fast turnaround proposals."
          ) { case (too, min, splits) => Create(scienceSubtype, too, min, partnerSplits = splits) }

        case ScienceSubtype.LargeProgram       =>
          toResult(
            (tooActivation, minPercentTime, minPercentTotal.toOption, totalTime.toOption).tupled,
            "'toOActivation', 'minPercentTime', 'minPercentTotal' and 'totalTime' are required for large program proposals."
          ) { case (too, min, minTotal, total) => Create(scienceSubtype, too, min, minTotal.some, total.some) }

        case ScienceSubtype.PoorWeather        =>
          Create(scienceSubtype, ToOActivation.None, ZeroPercent).success

        case ScienceSubtype.Queue              =>
          toResult(
            (tooActivation, minPercentTime, partnerSplits).tupled,
            "'toOActivation', 'minPercentTime' and 'partnerSplits' are required for queue proposals."
          ) { case (too, min, splits) => Create(scienceSubtype, too, min, partnerSplits = splits) }
      }
    }

  }

  object Edit {
    private def simpleEditBinding(s: ScienceSubtype): Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding.Option("toOActivation", rToo),
          IntPercentBinding.Option("minPercentTime", rMin)
        ) => (rToo, rMin).parMapN { (too, min) =>
          Edit(s, tooActivation = too, minPercentTime = min)
        }
      }

    private val Classical: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          IntPercentBinding.Option("minPercentTime", rMin),
          PartnerSplitsInput.Option("partnerSplits", rSplits)
        ) => (rMin, rSplits).parMapN { (min, splits) =>
          Edit(ScienceSubtype.Classical, minPercentTime = min, partnerSplits = splits)
        }
      }

    private val DemoScience: Matcher[Edit] =
      simpleEditBinding(ScienceSubtype.DemoScience)

    private val DirectorsTime: Matcher[Edit] =
      simpleEditBinding(ScienceSubtype.DirectorsTime)

    private val FastTurnaround: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding.Option("toOActivation", rToo),
          IntPercentBinding.Option("minPercentTime", rMin),
          TagBinding.Option("piAffiliation", rPartner)
        ) => (rToo, rMin, rPartner).parMapN { (too, min, partner) =>
          Edit(ScienceSubtype.FastTurnaround, too, min, partnerSplits = partner.map(p => Map(p -> HundredPercent)))
        }
      }

    private val LargeProgram: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding.Option("toOActivation", rToo),
          IntPercentBinding.Option("minPercentTime", rMin),
          IntPercentBinding.Option("minPercentTotalTime", rMinTotal),
          TimeSpanInput.Binding.Option("totalTime", rTotal)
        ) => (rToo, rMin, rMinTotal, rTotal).parMapN { (too, min, minTotal, total) =>
          Edit(ScienceSubtype.LargeProgram, too, min, Nullable.orAbsent(minTotal), Nullable.orAbsent(total))
        }
      }

    private val PoorWeather: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          EnumBinding.Option("ignore", rIgnore)
        ) => rIgnore.as(Edit(ScienceSubtype.PoorWeather))
      }

    private val Queue: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding.Option("toOActivation", rToo),
          IntPercentBinding.Option("minPercentTime", rMin),
          PartnerSplitsInput.Option("partnerSplits", rSplits)
        ) => (rToo, rMin, rSplits).parMapN { (too, min, splits) =>
          Edit(ScienceSubtype.Queue, too, min, partnerSplits = splits)
        }
      }

    private val SystemVerification: Matcher[Edit] =
      simpleEditBinding(ScienceSubtype.SystemVerification)

    val Binding: Matcher[Edit] =
      binding(
        Classical,
        DemoScience,
        DirectorsTime,
        FastTurnaround,
        LargeProgram,
        PoorWeather,
        Queue,
        SystemVerification
      )
  }

  private def binding[A](
    classical:          Matcher[A],
    demoScience:        Matcher[A],
    directorsTime:      Matcher[A],
    fastTurnaround:     Matcher[A],
    largeProgram:       Matcher[A],
    poorWeather:        Matcher[A],
    queue:              Matcher[A],
    systemVerification: Matcher[A]
  ): Matcher[A] =
    ObjectFieldsBinding.rmap {
      case List(
        classical.Option("classical", rClassical),
        demoScience.Option("demoScience", rDemo),
        directorsTime.Option("directorsTime", rDirector),
        fastTurnaround.Option("fastTurnaround", rFast),
        largeProgram.Option("largeProgram", rLarge),
        poorWeather.Option("poorWeather", rPoor),
        queue.Option("queue", rQueue),
        systemVerification.Option("systemVerification", rSystem)
      ) => (rClassical, rDemo, rDirector, rFast, rLarge, rPoor, rQueue, rSystem).parFlatMapN {
        (classical, demo, director, fast, large, poor, queue, system) =>
          val typeOption = List(classical, demo, director, fast, large, poor, queue, system).flatten
          typeOption match {
            case Nil      => Matcher.validationFailure(s"One of $formattedFieldNames must be provided.")
            case a :: Nil => a.success
            case _        => Matcher.validationFailure(s"Only one of $formattedFieldNames may be provided.")
          }
      }
    }

}