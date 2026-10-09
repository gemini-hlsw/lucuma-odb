// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.ElevationRange
import lucuma.odb.graphql.binding.*

final case class ElevationRangeInput(
  airMass:   Option[AirMassRangeInput],
  hourAngle: Option[HourAngleRangeInput]
) {

  def create: Result[ElevationRange] =
    atMostOne[Result[ElevationRange]](
      airMass.map(_.create)   -> "airMass",
      hourAngle.map(_.create) -> "hourAngle"
    ).flatMap(_.getOrElse(Result(ElevationRange.ByAirMass.Default)))

}

object ElevationRangeInput {

  val Default: ElevationRangeInput =
    ElevationRangeInput(
      AirMassRangeInput.Default.some,
      none
    )


  val Binding: Matcher[ElevationRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AirMassRangeInput.Binding.Option("airMass", rAir),
        HourAngleRangeInput.Binding.Option("hourAngle", rHour)
      ) => (rAir, rHour).parMapN(ElevationRangeInput(_, _)).flatTap: e =>
        atMostOne(e.airMass.void -> "airMass", e.hourAngle.void -> "hourAngle")
    }

}
