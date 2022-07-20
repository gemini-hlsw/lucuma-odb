// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input

import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.parallel._
import edu.gemini.grackle.Result
import lucuma.core.model.ElevationRange.HourAngle
import lucuma.core.model.ElevationRange.HourAngle.DecimalHour
import lucuma.odb.graphql.util.Bindings._

final case class HourAngleRangeInput(
  minHours: Option[DecimalHour],
  maxHours: Option[DecimalHour]
) {

  def create: Result[HourAngle] =
    Result.fromOption[HourAngle](
      (minHours, maxHours)
        .tupled
        .flatMap(HourAngle.fromOrderedDecimalHours.getOption),
      "Creating an hour angle range requires specifying both minHours and maxHours where minHours < maxHours"
    )

  def edit: HourAngle => Result[HourAngle] = ha => {
    val mn = minHours.getOrElse(ha.minHours)
    val mx = maxHours.getOrElse(ha.maxHours)
    Result.fromOption[HourAngle](
      HourAngle.fromOrderedDecimalHours.getOption((mn, mx)),
      s"Editing the hour angle range as specified would create an invalid range: minHours ($mn) >= maxHours ($mx)"
    )
  }

}

object HourAngleRangeInput {

  val HourAngleDecimalHour: Matcher[DecimalHour] =
    BigDecimalBinding.emap { bd =>
      DecimalHour.from(bd).leftMap(m => s"Invalid Hour Angle constraint: $bd: $m")
    }

  val SimpleBinding: Matcher[HourAngleRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        HourAngleDecimalHour.Option("min", rMin),
        HourAngleDecimalHour.Option("max", rMax)
      ) => (rMin, rMax).parMapN(HourAngleRangeInput(_, _))
    }

  val CreateBinding: Matcher[HourAngle] =
    SimpleBinding.rmap(_.create)

  val EditBinding: Matcher[HourAngle => Result[HourAngle]] =
    SimpleBinding.map(_.edit)

}
