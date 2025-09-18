// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.ElevationRange
import lucuma.core.model.HourAngleBound
import lucuma.odb.graphql.binding.*

final case class HourAngleRangeInput(
  minHours: Option[HourAngleBound],
  maxHours: Option[HourAngleBound]
) {

  def minBigDecimal: Option[BigDecimal] =
    minHours.map(_.toBigDecimal)

  def maxBigDecimal: Option[BigDecimal] =
    maxHours.map(_.toBigDecimal)

  def create: Result[ElevationRange.ByHourAngle] =
    Result.fromOption(
      (minHours, maxHours).tupled
        .flatMap(ElevationRange.ByHourAngle.FromOrderedBounds.getOption),
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
      ElevationRange.ByHourAngle.DefaultMin.some,
      ElevationRange.ByHourAngle.DefaultMax.some
    )

  val HourAngleDecimalHour: Matcher[HourAngleBound] =
    BigDecimalBinding.emap { bd =>
      HourAngleBound.from(bd).leftMap(m => s"Invalid Hour Angle constraint: $bd: $m")
    }

  val Binding: Matcher[HourAngleRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
            HourAngleDecimalHour.Option("minHours", rMin),
            HourAngleDecimalHour.Option("maxHours", rMax)
          ) =>
        (rMin, rMax).parMapN(HourAngleRangeInput(_, _))
    }

}
