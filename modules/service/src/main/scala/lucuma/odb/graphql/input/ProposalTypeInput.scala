// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import lucuma.core.model.CallForProposals
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
    callId:          Option[CallForProposals.Id],
    tooActivation:   ToOActivation,
    minPercentTime:  IntPercent,
    minPercentTotal: Option[IntPercent]   = None,
    totalTime:       Option[TimeSpan]     = None,
    partnerSplits:   Map[Tag, IntPercent] = Map.empty
  ) {
    def asEdit: Edit =
      Edit(
        scienceSubtype,
        Nullable.orNull(callId),
        tooActivation.some,
        minPercentTime.some,
        Nullable.orNull(minPercentTotal),
        Nullable.orNull(totalTime),
        partnerSplits.some
      )
  }

  object Create {

    val Default: Create =
      Create(ScienceSubtype.Queue, none, ToOActivation.None, ZeroPercent)

    private def simpleCreateBinding(s: ScienceSubtype): Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Option("callId", rCid),
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin)
        ) => (rCid, rToo, rMin).parMapN { (cid, too, min) => Create(s, cid, too, min) }
      }

    private val Classical: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Option("callId", rCid),
          IntPercentBinding("minPercentTime", rMin),
          PartnerSplitsInput.Option("partnerSplits", rSplits)
        ) => (rCid, rMin, rSplits).parMapN { (cid, min, splits) =>
          Create(ScienceSubtype.Classical, cid, ToOActivation.None, min, partnerSplits = splits.getOrElse(Map.empty))
        }
      }

    private val DemoScience: Matcher[Create] =
      simpleCreateBinding(ScienceSubtype.DemoScience)

    private val DirectorsTime: Matcher[Create] =
      simpleCreateBinding(ScienceSubtype.DirectorsTime)

    private val FastTurnaround: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Option("callId", rCid),
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          TagBinding("piAffiliation", rPartner)
        ) => (rCid, rToo, rMin, rPartner).parMapN { (cid, too, min, partner) =>
          Create(ScienceSubtype.FastTurnaround, cid, too, min, partnerSplits = Map(partner -> HundredPercent))
        }
      }

    private val LargeProgram: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Option("callId", rCid),
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          IntPercentBinding("minPercentTotalTime", rMinTotal),
          TimeSpanInput.Binding("totalTime", rTotal)
        ) => (rCid, rToo, rMin, rMinTotal, rTotal).parMapN { (cid, too, min, minTotal, total) =>
          Create(ScienceSubtype.LargeProgram, cid, too, min, minTotal.some, total.some)
        }
      }

    private val PoorWeather: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Option("callId", rCid)
        ) => rCid.map { cid =>
          Create(ScienceSubtype.PoorWeather, cid, ToOActivation.None, ZeroPercent)
        }
      }

    private val Queue: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Option("callId", rCid),
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          PartnerSplitsInput("partnerSplits", rSplits)
        ) => (rCid, rToo, rMin, rSplits).parMapN { (cid, too, min, splits) =>
          Create(ScienceSubtype.Queue, cid, too, min, partnerSplits = splits)
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
    callId:          Nullable[CallForProposals.Id],
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
          ) { case (min, splits) => Create(scienceSubtype, callId.toOption, ToOActivation.None, min, partnerSplits = splits) }

        case ScienceSubtype.DemoScience   |
             ScienceSubtype.DirectorsTime |
             ScienceSubtype.SystemVerification =>
          toResult(
            (tooActivation, minPercentTime).tupled,
            s"'toOActivation' and 'minPercentTime' are required for ${scienceSubtype.tag.replace('_', ' ')} proposals."
          ) { case (too, min) => Create(scienceSubtype, callId.toOption, too, min) }

        case ScienceSubtype.FastTurnaround     =>
          toResult(
            (tooActivation, minPercentTime, partnerSplits).tupled,
            "'toOActivation', 'minPercentTime' and 'piAffiliate' are required for fast turnaround proposals."
          ) { case (too, min, splits) => Create(scienceSubtype, callId.toOption, too, min, partnerSplits = splits) }

        case ScienceSubtype.LargeProgram       =>
          toResult(
            (tooActivation, minPercentTime, minPercentTotal.toOption, totalTime.toOption).tupled,
            "'toOActivation', 'minPercentTime', 'minPercentTotal' and 'totalTime' are required for large program proposals."
          ) { case (too, min, minTotal, total) => Create(scienceSubtype, callId.toOption, too, min, minTotal.some, total.some) }

        case ScienceSubtype.PoorWeather        =>
          Create(scienceSubtype, callId.toOption, ToOActivation.None, ZeroPercent).success

        case ScienceSubtype.Queue              =>
          toResult(
            (tooActivation, minPercentTime, partnerSplits).tupled,
            "'toOActivation', 'minPercentTime' and 'partnerSplits' are required for queue proposals."
          ) { case (too, min, splits) => Create(scienceSubtype, callId.toOption, too, min, partnerSplits = splits) }
      }
    }

  }

  object Edit {
    private def simpleEditBinding(s: ScienceSubtype): Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Nullable("callId", rCid),
          ToOActivationBinding.Option("toOActivation", rToo),
          IntPercentBinding.Option("minPercentTime", rMin)
        ) => (rCid, rToo, rMin).parMapN { (cid, too, min) =>
          Edit(s, cid, tooActivation = too, minPercentTime = min)
        }
      }

    private val Classical: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Nullable("callId", rCid),
          IntPercentBinding.Option("minPercentTime", rMin),
          PartnerSplitsInput.Option("partnerSplits", rSplits)
        ) => (rCid, rMin, rSplits).parMapN { (cid, min, splits) =>
          Edit(ScienceSubtype.Classical, cid, minPercentTime = min, partnerSplits = splits)
        }
      }

    private val DemoScience: Matcher[Edit] =
      simpleEditBinding(ScienceSubtype.DemoScience)

    private val DirectorsTime: Matcher[Edit] =
      simpleEditBinding(ScienceSubtype.DirectorsTime)

    private val FastTurnaround: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Nullable("callId", rCid),
          ToOActivationBinding.Option("toOActivation", rToo),
          IntPercentBinding.Option("minPercentTime", rMin),
          TagBinding.Option("piAffiliation", rPartner)
        ) => (rCid, rToo, rMin, rPartner).parMapN { (cid, too, min, partner) =>
          Edit(ScienceSubtype.FastTurnaround, cid, too, min, partnerSplits = partner.map(p => Map(p -> HundredPercent)))
        }
      }

    private val LargeProgram: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Nullable("callId", rCid),
          ToOActivationBinding.Option("toOActivation", rToo),
          IntPercentBinding.Option("minPercentTime", rMin),
          IntPercentBinding.Option("minPercentTotalTime", rMinTotal),
          TimeSpanInput.Binding.Option("totalTime", rTotal)
        ) => (rCid, rToo, rMin, rMinTotal, rTotal).parMapN { (cid, too, min, minTotal, total) =>
          Edit(ScienceSubtype.LargeProgram, cid, too, min, Nullable.orAbsent(minTotal), Nullable.orAbsent(total))
        }
      }

    private val PoorWeather: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Nullable("callId", rCid)
        ) => rCid.map { cid => Edit(ScienceSubtype.PoorWeather, cid) }
      }

    private val Queue: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding.Nullable("callId", rCid),
          ToOActivationBinding.Option("toOActivation", rToo),
          IntPercentBinding.Option("minPercentTime", rMin),
          PartnerSplitsInput.Option("partnerSplits", rSplits)
        ) => (rCid, rToo, rMin, rSplits).parMapN { (cid, too, min, splits) =>
          Edit(ScienceSubtype.Queue, cid, too, min, partnerSplits = splits)
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