// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.eq.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.query.given
import lucuma.odb.json.timeaccounting.given

import scala.collection.immutable.SortedSet

object TimeCharge {

  enum DiscountDiscriminator(val dbTag: String) {
    case Daylight extends DiscountDiscriminator("daylight")
    case Fault    extends DiscountDiscriminator("fault")
    case Qa       extends DiscountDiscriminator("qa")
    case Weather  extends DiscountDiscriminator("weather")
  }

  object DiscountDiscriminator {

    given Enumerated[DiscountDiscriminator] =
      Enumerated.from(
        Daylight,
        Fault,
        Qa,
        Weather
      ).withTag(_.dbTag)

  }

  case class Discount(
    interval: TimestampInterval,
    partner:  TimeSpan,
    program:  TimeSpan,
    atoms:    SortedSet[Atom.Id],
    comment:  String
  )

  object Discount {

    given Eq[Discount] =
      Eq.by { a => (a.interval, a.partner, a.program, a.atoms, a.comment) }

    given Encoder[Discount] =
      Encoder.instance { a =>
        Json.obj(
          "start"   -> a.interval.start.asJson,
          "end"     -> a.interval.end.asJson,
          "partner" -> a.partner.asJson,
          "program" -> a.program.asJson,
          "comment" -> a.comment.asJson
        )
      }
  }

  sealed trait DiscountEntry extends Product with Serializable {
    def discount:      Discount
    def discriminator: DiscountDiscriminator
  }

  object DiscountEntry {

    case class Daylight(
      discount: Discount,
      site:     Site
    ) extends DiscountEntry {
      override def discriminator: DiscountDiscriminator =
        DiscountDiscriminator.Daylight
    }

    object Daylight {

      given Eq[Daylight] =
        Eq.by { a => (a.discount, a.site) }

    }

    given Eq[DiscountEntry] =
      Eq.instance {
        case (a@Daylight(_, _), b@Daylight(_, _)) => a === b
        case _                                    => false
      }

    given Encoder[DiscountEntry] =
      Encoder.instance {
        case Daylight(discount, site) => discount.asJson.deepMerge(Json.obj("site" -> site.asJson))
      }

  }

  case class Invoice(
    executionTime: CategorizedTime,
    discounts:     List[DiscountEntry],
    finalCharge:   CategorizedTime
  )

  object Invoice {
    val Empty: Invoice =
      Invoice(CategorizedTime.Zero, Nil, CategorizedTime.Zero)

    given Eq[Invoice] =
      Eq.by { a => (a.executionTime, a.discounts, a.finalCharge) }

    given Encoder[Invoice] =
      Encoder.instance { a =>
        Json.obj(
          "executionTime" -> a.executionTime.asJson,
          "discounts"     -> a.discounts.asJson,
          "finalCharge"   -> a.finalCharge.asJson
        )
      }

  }

}
