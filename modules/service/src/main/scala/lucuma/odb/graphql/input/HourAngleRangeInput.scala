// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import eu.timepit.refined.api.Refined.value
import grackle.Result
import lucuma.core.model.ElevationRange.HourAngle
import lucuma.core.model.ElevationRange.HourAngle.DecimalHour
import lucuma.odb.graphql.binding.*

final case class HourAngleRangeInput(
  minHours: Option[DecimalHour],
  maxHours: Option[DecimalHour]
) {

  def minBigDecimal: Option[BigDecimal] =
    minHours.map(_.value)

  def maxBigDecimal: Option[BigDecimal] =
    maxHours.map(_.value)

  def create: Result[HourAngle] =
    Result.fromOption(
      (minHours, maxHours)
        .tupled
        .flatMap(HourAngle.fromOrderedDecimalHours.getOption),
      HourAngleRangeInput.messages.BothMinAndMax
    )

}

object HourAngleRangeInput {

  object messages {
    val BothMinAndMax: String =
      "Creating an hour angle range requires specifying both minHours and maxHours where minHours < maxHours"
  }

  val Default: HourAngleRangeInput =
    HourAngleRangeInput(
      HourAngle.DefaultMin.some,
      HourAngle.DefaultMax.some
    )

  val HourAngleDecimalHour: Matcher[DecimalHour] =
    BigDecimalBinding.emap { bd =>
      DecimalHour.from(bd).leftMap(m => s"Invalid Hour Angle constraint: $bd: $m")
    }

  val Binding: Matcher[HourAngleRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        HourAngleDecimalHour.Option("minHours", rMin),
        HourAngleDecimalHour.Option("maxHours", rMax)
      ) => (rMin, rMax).parMapN(HourAngleRangeInput(_, _))
    }

}
