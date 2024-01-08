// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.eq.*
import lucuma.core.enums.Site
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval

/**
 * Collection of data related to time accounting.
 */
object TimeCharge {

  /**
   * Enumeration of time charge discount types.
   */
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

  /**
   * Time accounting discount that will be subtracted from raw execution time.
   */
  case class Discount(
    interval: TimestampInterval,
    partner:  TimeSpan,
    program:  TimeSpan,
    comment:  String
  )

  object Discount {

    given Eq[Discount] =
      Eq.by { a => (a.interval, a.partner, a.program, a.comment) }

  }

  /**
   * ADT of time charge discount types.
   */
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

    case class Qa(
      discount: Discount,
      datasets: Set[Dataset.Id]
    ) extends DiscountEntry {
      override def discriminator: DiscountDiscriminator =
        DiscountDiscriminator.Qa
    }

    object Qa {

      given Eq[Qa] =
        Eq.by { a => (a.discount, a.datasets) }

    }

    given Eq[DiscountEntry] =
      Eq.instance {
        case (a@Daylight(_, _), b@Daylight(_, _)) => a === b
        case (a@Qa(_, _),       b@Qa(_, _))       => a === b
        case _                                    => false
      }

  }

  /**
   * The result of a time accounting calculation.  Execution time, minus
   * discounts, produces the final charge.
   */
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

  }

}
