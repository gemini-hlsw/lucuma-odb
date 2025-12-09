// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.model.Ephemeris
import lucuma.core.model.SourceProfile
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.sourceprofile.SourceProfileInput

object TargetPropertiesInput {

  final case class Create(
    name: NonEmptyString,
    subtypeInfo: SiderealInput.Create | Ephemeris.Key | OpportunityInput.Create,
    sourceProfile: SourceProfile,
    existence: Existence
  )

  final case class Edit(
    name: Option[NonEmptyString],
    subtypeInfo: Option[SiderealInput.Edit | Ephemeris.Key | OpportunityInput.Edit],
    sourceProfile: Option[SourceProfile => Result[SourceProfile]],
    existence: Option[Existence]
  )

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.NonNullable("name", rName),
        SiderealInput.EditBinding.Option("sidereal", rSidereal),
        NonsiderealInput.Binding.Option("nonsidereal", rNonsidereal),
        OpportunityInput.EditBinding.Option("opportunity", rOpportunity),
        SourceProfileInput.EditBinding.Option("sourceProfile", rSourceProfile),
        ExistenceBinding.Option("existence", rExistence)
      ) => (rName, rSidereal, rNonsidereal, rOpportunity, rSourceProfile, rExistence).parTupled.flatMap {
        case (name, sidereal, nonsidereal, opportunity, sourceProfile, existence) =>
          (sidereal, nonsidereal, opportunity) match {
            case (Some(s), None, None) => Result(Some(s))
            case (None, Some(n), None) => Result(Some(n))
            case (None, None, Some(o)) => Result(Some(o))
            case (None, None, None)    => Result(None)
            case _ => Matcher.validationFailure("At most one of sidereal, nonsidereal, or opportunity may be specified (found multiple).")
          } map  { Edit(name, _, sourceProfile, existence) }
      } 
    }

  val Binding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        SiderealInput.CreateBinding.Option("sidereal", rSidereal),
        NonsiderealInput.Binding.Option("nonsidereal", rNonsidereal),
        OpportunityInput.CreateBinding.Option("opportunity", rOpportunity),
        SourceProfileInput.CreateBinding.Option("sourceProfile", rSourceProfile),
        ExistenceBinding.Option("existence", rExistence)
      ) => (rName, rSidereal, rNonsidereal, rOpportunity, rSourceProfile, rExistence).parTupled.flatMap {
        case (name, sidereal, nonsidereal, opportunity, sourceProfile, existence) =>
          (name, sourceProfile, sidereal, nonsidereal, opportunity) match {
            case (None, _, _, _, _)             => Matcher.validationFailure("Target name is required on creation.")
            case (_, None, _, _, _)             => Matcher.validationFailure("Source Profile is required on creation.")
            case (Some(t), Some(p), Some(s), None, None) => Result(Create(t, s, p, existence.getOrElse(Existence.Default)))
            case (Some(t), Some(p), None, Some(n), None) => Result(Create(t, n, p, existence.getOrElse(Existence.Default)))
            case (Some(t), Some(p), None, None, Some(o)) => Result(Create(t, o, p, existence.getOrElse(Existence.Default)))
            case _ => Matcher.validationFailure("Exactly one of sidereal, nonsidereal, or opportunity must be specified.")
          }
      }
    }
}
