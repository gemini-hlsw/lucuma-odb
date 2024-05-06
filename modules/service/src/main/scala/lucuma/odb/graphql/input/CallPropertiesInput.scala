// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import grackle.syntax.*
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import lucuma.core.model.CallForProposals
import lucuma.core.model.IntPercent
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

object CallPropertiesInput {

  private val fieldNames: List[String] =
    List(
      "demoScience",
      "directorsTime",
      "fastTurnaround",
      "poorWeather",
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
    PartnerSplitInput.Binding.List.rmap { splits =>
      val map = splits.map(a => (a.partner -> a.percent)).toMap

      Matcher
        .validationFailure("Each partner may only appear once.")
        .unlessA(splits.length === map.size) *>
      Matcher
        .validationFailure("Percentages must sum to exactly 100.")
        .unlessA(splits.foldMap(_.percent.value) === 100) *>
      map.success

    }

  case class Create(
    cfpId:           CallForProposals.Id,
    scienceSubtype:  ScienceSubtype,
    tooActivation:   ToOActivation,
    minPercentTime:  IntPercent,
    minPercentTotal: IntPercent           = ZeroPercent,
    totalTime:       TimeSpan             = TimeSpan.Zero,
    partnerSplits:   Map[Tag, IntPercent] = Map.empty
  )

  object Create {

    private def simpleCallCreateBinding(s: ScienceSubtype): Matcher[CallForProposals.Id => Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin)
        ) => (rToo, rMin).parMapN { (too, min) => Create(_, s, too, min) }
      }

    private val Classical: Matcher[CallForProposals.Id => Create] =
      ObjectFieldsBinding.rmap {
        case List(
          IntPercentBinding("minPercentTime", rMin),
          PartnerSplitsInput("partnerSplits", rSplits)
        ) => (rMin, rSplits).parMapN { (min, split) =>
          Create(_, ScienceSubtype.Classical, ToOActivation.None, min, partnerSplits = split)
        }
      }

    private val DemoScience: Matcher[CallForProposals.Id => Create] =
      simpleCallCreateBinding(ScienceSubtype.DemoScience)

    private val DirectorsTime: Matcher[CallForProposals.Id => Create] =
      simpleCallCreateBinding(ScienceSubtype.DirectorsTime)

    private val FastTurnaround: Matcher[CallForProposals.Id => Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          TagBinding("piAffiliation", rPartner)
        ) => (rToo, rMin, rPartner).parMapN { (too, min, partner) =>
          Create(_, ScienceSubtype.FastTurnaround, too, min, partnerSplits = Map(partner -> HundredPercent))
        }
      }

    private val LargeProgram: Matcher[CallForProposals.Id => Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          IntPercentBinding("minPercentTotalTime", rMinTotal),
          TimeSpanInput.Binding("totalTime", rTotal)
        ) => (rToo, rMin, rMinTotal, rTotal).parMapN { (too, min, minTotal, total) =>
          Create(_, ScienceSubtype.LargeProgram, too, min, minTotal, total)
        }
      }

    private val PoorWeather: Matcher[CallForProposals.Id => Create] =
      ObjectFieldsBinding.rmap {
        case Nil =>
          (cid => Create(cid, ScienceSubtype.PoorWeather, ToOActivation.None, ZeroPercent)).success
      }

    private val Queue: Matcher[CallForProposals.Id => Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ToOActivationBinding("toOActivation", rToo),
          IntPercentBinding("minPercentTime", rMin),
          PartnerSplitsInput("partnerSplits", rSplits)
        ) => (rToo, rMin, rSplits).parMapN { (too, min, split) =>
          Create(_, ScienceSubtype.Queue, too, min, partnerSplits = split)
        }
      }

    private val SystemVerification: Matcher[CallForProposals.Id => Create] =
      simpleCallCreateBinding(ScienceSubtype.DirectorsTime)

    val binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsIdBinding("callId", rCid),
          Classical.Option("classical", rClassical),
          DemoScience.Option("demoScience", rDemo),
          DirectorsTime.Option("directorsTime", rDirector),
          FastTurnaround.Option("fastTurnaround", rFast),
          LargeProgram.Option("largeProgram", rLarge),
          PoorWeather.Option("poorWeather", rPoor),
          Queue.Option("queue", rQueue),
          SystemVerification.Option("systemVerification", rSystem)
        ) => (rCid, rClassical, rDemo, rDirector, rFast, rLarge, rPoor, rQueue, rSystem).parFlatMapN {
          (cid, classical, demo, director, fast, large, poor, queue, system) =>
            List(
              classical,
              demo,
              director,
              fast,
              large,
              poor,
              queue,
              system
            ).flatMap(_.map(_.apply(cid)).toList) match {
              case Nil      => Matcher.validationFailure(s"One of $formattedFieldNames must be provided.")
              case c :: Nil => c.success
              case _        => Matcher.validationFailure(s"Only one of $formattedFieldNames must be provided.")
            }
        }
      }
  }

}