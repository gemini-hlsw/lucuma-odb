// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.flatMap.*
import cats.syntax.option.*
import grackle.Result
import lucuma.core.model.ElevationRange
import lucuma.odb.graphql.binding.*

final case class ElevationRangeInput(
  airMass:   Option[AirMassRangeInput],
  hourAngle: Option[HourAngleRangeInput]
) {

  def create: Result[ElevationRange] =
    oneOrDefault(Result(ElevationRange.ByAirMass.Default))(
      airMass.map(_.create)   -> "airMass",
      hourAngle.map(_.create) -> "hourAngle"
    ).flatten

}

object ElevationRangeInput {

  val Default: ElevationRangeInput =
    ElevationRangeInput(
      AirMassRangeInput.Default.some,
      none
    )


  val Binding: Matcher[ElevationRangeInput] =
    OneOfOrDefaultBinding(ElevationRangeInput(none, none))(
      "airMass"   -> AirMassRangeInput.Binding.map(a => ElevationRangeInput(a.some, none)),
      "hourAngle" -> HourAngleRangeInput.Binding.map(h => ElevationRangeInput(none, h.some))
    )

}
