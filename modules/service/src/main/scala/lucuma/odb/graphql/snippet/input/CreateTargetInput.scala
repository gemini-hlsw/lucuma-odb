// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import lucuma.core.model.EphemerisKey
import lucuma.odb.graphql.util.Bindings._

final case class CreateTargetInput(
  name: NonEmptyString,
  tracking: Either[SiderealInput, EphemerisKey],
  sourceProfile: Json
)

object CreateTargetInput {
  val Binding: Matcher[CreateTargetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding("name", rName),
        SiderealInput.Binding.Option("sidereal", rSidereal),
        NonsiderealInput.Binding.Option("nonsidereal", rNonsidereal),
        ObjectAsJsonBinding("sourceProfile", rSourceProfile)
      ) => (rName, rSidereal, rNonsidereal, rSourceProfile).parTupled.flatMap {
        case (name, sidereal, nonsidereal, sourceProfile) =>
          (sidereal, nonsidereal) match {
            case (Some(s), None)    => Result(CreateTargetInput(name, Left(s), sourceProfile))
            case (None, Some(n))    => Result(CreateTargetInput(name, Right(n), sourceProfile))
            case (Some(_), Some(_)) => Result.failure("Found both sidereal and nonsidereal tracking; only one may be provided.")
            case (None, None)       => Result.failure("Found neither sidereal noe nonsidereal tracking; exactly one must be provided.")
          }
      }
    }
}
