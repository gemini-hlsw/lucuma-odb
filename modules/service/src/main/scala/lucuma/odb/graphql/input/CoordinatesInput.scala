// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import edu.gemini.grackle.Result
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

//# Absolute coordinates relative base epoch
//input CoordinatesInput {
//  ra: RightAscensionInput
//  dec: DeclinationInput
//}

final case class CoordinatesInput(
  ra:  Option[RightAscension],
  dec: Option[Declination]
) {

  def create: Result[Option[Coordinates]] =
    (ra, dec) match {
      case (Some(r), Some(d)) => Result(Coordinates(r, d).some)
      case (None, None)       => Result(none)
      case _                  => Result.failure(CoordinatesInput.messages.BothRaAndDecNeeded)
    }

}

object CoordinatesInput {

  object messages {
    val BothRaAndDecNeeded: String =
      "Both ra and dec are required in order to specify a coordinate."
  }

  val Binding: Matcher[CoordinatesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        RightAscensionInput.Binding.Option("ra", rRa),
        DeclinationInput.Binding.Option("dec", rDec)
      ) => (rRa, rDec).parMapN(CoordinatesInput(_, _))
    }
}
