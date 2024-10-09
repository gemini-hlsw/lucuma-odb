// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.order.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

import java.time.temporal.ChronoUnit.DAYS

case class TimeChargeCorrectionInput(
  chargeClass: ChargeClass,
  op:          TimeChargeCorrection.Op,
  amount:      TimeSpan,
  comment:     Option[NonEmptyString]
)

object TimeChargeCorrectionInput {

  /**
   * An arbitrary, but nevertheless large, limit on the size of time charge
   * corrections.
   */
  val CorrectionLimit: TimeSpan =
    TimeSpan.unsafeFromDuration(365, DAYS)

  val Binding: Matcher[TimeChargeCorrectionInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ChargeClassBinding("chargeClass", rChargeClass),
        TimeChargeCorrectionOpBinding("op", rOp),
        TimeSpanInput.Binding("amount", rAmount),
        NonEmptyStringBinding.Option("comment", rComment)
      ) =>
        // We're limiting the time charge correction. Time accounting values are
        // summed and can theoretically overflow.  The only realistic way to
        // have this happen though is via mistakes in adding time charge
        // corrections. (The maximum 'interval' value is 178,000,000 years.)
        val rAmountʹ = rAmount.flatMap(m => Result.fromEither(Either.cond(
          m <= CorrectionLimit,
          m,
          "Time charge correction values over 365 days are not permitted."
        )))
        (rChargeClass, rOp, rAmountʹ, rComment).parMapN: (chargeClass, op, amount, comment) =>
          TimeChargeCorrectionInput(chargeClass, op, amount, comment)
    }

}
