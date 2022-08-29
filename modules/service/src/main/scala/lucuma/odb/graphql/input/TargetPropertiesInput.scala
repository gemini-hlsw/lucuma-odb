// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.EphemerisKey
import lucuma.core.model.SourceProfile
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.sourceprofile.SourceProfileInput

final case class TargetPropertiesInput(
  name: NonEmptyString,
  tracking: Either[SiderealInput, EphemerisKey],
  sourceProfile: SourceProfile,
  existence: Existence
)

object TargetPropertiesInput {

  val Binding: Matcher[TargetPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        SiderealInput.Binding.Option("sidereal", rSidereal),
        NonsiderealInput.Binding.Option("nonsidereal", rNonsidereal),
        SourceProfileInput.CreateBinding.Option("sourceProfile", rSourceProfile),
        ExistenceBinding.Option("existence", rExistence)
      ) => (rName, rSidereal, rNonsidereal, rSourceProfile, rExistence).parTupled.flatMap {
        case (name, sidereal, nonsidereal, sourceProfile, existence) =>
          (name, sourceProfile, sidereal, nonsidereal) match {
            case (None, _, _, _)             => Result.failure("Target name is required on creation.")
            case (_, None, _, _)             => Result.failure("Source Profile is required on creation.")
            case (Some(t), Some(p), Some(s), None) => Result(TargetPropertiesInput(t, Left(s), p, existence.getOrElse(Existence.Default)))
            case (Some(t), Some(p), None, Some(n)) => Result(TargetPropertiesInput(t, Right(n), p, existence.getOrElse(Existence.Default)))
            case (_, _, Some(_), Some(_))    => Result.failure("Found both sidereal and nonsidereal tracking; only one may be provided.")
            case (_, _, None, None)          => Result.failure("Found neither sidereal noe nonsidereal tracking; exactly one must be provided.")
          }
      }
    }
}
