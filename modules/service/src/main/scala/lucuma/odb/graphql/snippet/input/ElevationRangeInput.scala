// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input

import cats.syntax.parallel._
import edu.gemini.grackle.Result
import lucuma.core.model.ElevationRange
import lucuma.core.model.ElevationRange.{AirMass, HourAngle}
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

  def edit: ElevationRange => Result[ElevationRange] = er =>
    (er, airMass, hourAngle) match {
      case (a@AirMass(_, _),   Some(am), None    ) => am.edit(a)
      case (_,                 Some(am), None    ) => am.create
      case (h@HourAngle(_, _), None,     Some(hr)) => hr.edit(h)
      case (_,                 None,     Some(hr)) => hr.create
      case (_,                 None,     None    ) => Result(er)
      case (_,                 Some(_),  Some(_) ) => OnlyOneFailure
    }

}

object ElevationRangeInput {

  private def OnlyOneFailure[A]: Result[A] =
    Result.failure[A]("Only one of airMass or hourAngle may be specified.")

  val SimpleBinding: Matcher[ElevationRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AirMassRangeInput.SimpleBinding.Option("airMass", rAir),
        HourAngleRangeInput.SimpleBinding.Option("hourAngle", rHour)
      ) => (rAir, rHour).parMapN(ElevationRangeInput(_, _))
    }

  val CreateBinding: Matcher[ElevationRange] =
    SimpleBinding.rmap(_.create)

  val EditBinding: Matcher[ElevationRange => Result[ElevationRange]] =
    SimpleBinding.map(_.edit)

}
