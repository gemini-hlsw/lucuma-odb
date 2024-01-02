// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

case class TimeChargeCorrectionInput(
  chargeClass: ChargeClass,
  op:          TimeChargeCorrection.Op,
  amount:      TimeSpan,
  comment:     Option[NonEmptyString]
)

object TimeChargeCorrectionInput {

  val Binding: Matcher[TimeChargeCorrectionInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ChargeClassBinding("chargeClass", rChargeClass),
        TimeChargeCorrectionOpBinding("op", rOp),
        TimeSpanInput.Binding("amount", rAmount),
        NonEmptyStringBinding.Option("comment", rComment)
      ) => (rChargeClass, rOp, rAmount, rComment).parMapN { (chargeClass, op, amount, comment) =>
        TimeChargeCorrectionInput(chargeClass, op, amount, comment)
      }
    }

}
