// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.EphemerisKey
import lucuma.core.model.SourceProfile
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.*
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.sourceprofile.SourceProfileInput


object TargetPropertiesInput {

  final case class Create(
    name: NonEmptyString,
    tracking: Either[SiderealInput.Create, EphemerisKey],
    sourceProfile: SourceProfile,
    existence: Existence
  )

  final case class Edit(
    name: Nullable[NonEmptyString], // can set name to null, oddly
    tracking: Option[Either[SiderealInput.Edit, EphemerisKey]],
    sourceProfile: Option[SourceProfile => Result[SourceProfile]],
    existence: Option[Existence]
  )

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("name", rName),
        SiderealInput.EditBinding.Option("sidereal", rSidereal),
        NonsiderealInput.Binding.Option("nonsidereal", rNonsidereal),
        SourceProfileInput.EditBinding.Option("sourceProfile", rSourceProfile),
        ExistenceBinding.Option("existence", rExistence)
      ) => (rName, rSidereal, rNonsidereal, rSourceProfile, rExistence).parTupled.flatMap {
        case (name, sidereal, nonsidereal, sourceProfile, existence) =>
          (sidereal, nonsidereal) match {
            case (Some(_), Some(_))  => Result.failure("Found both sidereal and nonsidereal tracking; at most one may be provided.")
            case (a, b)              => Result(a.map(_.asLeft) orElse b.map(_.asRight))
          } map  { Edit(name, _, sourceProfile, existence) }
      } 
    }

  val Binding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        SiderealInput.CreateBinding.Option("sidereal", rSidereal),
        NonsiderealInput.Binding.Option("nonsidereal", rNonsidereal),
        SourceProfileInput.CreateBinding.Option("sourceProfile", rSourceProfile),
        ExistenceBinding.Option("existence", rExistence)
      ) => (rName, rSidereal, rNonsidereal, rSourceProfile, rExistence).parTupled.flatMap {
        case (name, sidereal, nonsidereal, sourceProfile, existence) =>
          (name, sourceProfile, sidereal, nonsidereal) match {
            case (None, _, _, _)             => Result.failure("Target name is required on creation.")
            case (_, None, _, _)             => Result.failure("Source Profile is required on creation.")
            case (Some(t), Some(p), Some(s), None) => Result(Create(t, Left(s), p, existence.getOrElse(Existence.Default)))
            case (Some(t), Some(p), None, Some(n)) => Result(Create(t, Right(n), p, existence.getOrElse(Existence.Default)))
            case (_, _, Some(_), Some(_))    => Result.failure("Found both sidereal and nonsidereal tracking; only one may be provided.")
            case (_, _, None, None)          => Result.failure("Found neither sidereal noe nonsidereal tracking; exactly one must be provided.")
          }
      }
    }
}
