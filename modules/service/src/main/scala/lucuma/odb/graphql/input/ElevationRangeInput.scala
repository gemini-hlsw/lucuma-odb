// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import edu.gemini.grackle.Result
import lucuma.core.model.ElevationRange
import lucuma.core.model.ElevationRange.AirMass
import lucuma.odb.graphql.binding._

final case class ElevationRangeInput(
  airMass:   Option[AirMassRangeInput],
  hourAngle: Option[HourAngleRangeInput]
) {

  def create: Result[ElevationRange] =
    (airMass, hourAngle) match {
      case (Some(am), None)   => am.create
      case (None, Some(hr))   => hr.create
      case (None, None)       => Result(AirMass.Default)
      case (Some(_), Some(_)) => Result.failure(ElevationRangeInput.messages.OnlyOneDefinition)
    }

}

object ElevationRangeInput {

  val Default: ElevationRangeInput =
    ElevationRangeInput(
      AirMassRangeInput.Default.some,
      none
    )

  object messages {
    val OnlyOneDefinition: String = "Only one of airMass or hourAngle may be specified."
  }

  val Binding: Matcher[ElevationRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AirMassRangeInput.Binding.Option("airMass", rAir),
        HourAngleRangeInput.Binding.Option("hourAngle", rHour)
      ) => (rAir, rHour).parMapN(ElevationRangeInput(_, _)).flatMap {
        case ElevationRangeInput(Some(_), Some(_)) => Result.failure[ElevationRangeInput](messages.OnlyOneDefinition)
        case other                                 => Result(other)
      }
    }

}
