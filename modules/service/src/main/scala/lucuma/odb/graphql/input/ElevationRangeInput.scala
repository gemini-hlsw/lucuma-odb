// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.parallel._
import edu.gemini.grackle.Result
import lucuma.core.model.ElevationRange
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.ElevationRange.HourAngle
import lucuma.odb.graphql.util.Bindings._

final case class ElevationRangeInput(
  airMass:   Option[AirMassRangeInput],
  hourAngle: Option[HourAngleRangeInput]
) {

  import ElevationRangeInput.OnlyOneFailure

  def create: Result[ElevationRange] =
    (airMass, hourAngle) match {
      case (Some(am), None)   => am.create
      case (None, Some(hr))   => hr.create
      case (None, None)       => Result(AirMass.Default)
      case (Some(_), Some(_)) => OnlyOneFailure
    }

}

object ElevationRangeInput {

  val Default: ElevationRangeInput =
    ElevationRangeInput(
      AirMassRangeInput.Default.some,
      none
    )

  private def OnlyOneFailure[A]: Result[A] =
    Result.failure[A]("Only one of airMass or hourAngle may be specified.")

  val Binding: Matcher[ElevationRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AirMassRangeInput.Binding.Option("airMass", rAir),
        HourAngleRangeInput.Binding.Option("hourAngle", rHour)
      ) => (rAir, rHour).parMapN(ElevationRangeInput(_, _)).flatMap {
        case ElevationRangeInput(Some(_), Some(_)) => OnlyOneFailure[ElevationRangeInput]
        case other                                 => Result(other)
      }
    }

}
