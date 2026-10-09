// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.syntax.*
import lucuma.core.model.SourceProfile
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.sourceprofile.SourceProfileInput

object TargetPropertiesInput {

  final case class Create(
    name: NonEmptyString,
    subtypeInfo: SiderealInput.Create | NonsiderealInput.Create | OpportunityInput.Create,
    sourceProfile: SourceProfile,
    existence: Existence
  )

  final case class Edit(
    name: Option[NonEmptyString],
    subtypeInfo: Option[SiderealInput.Edit | NonsiderealInput.Edit | OpportunityInput.Edit],
    sourceProfile: Option[SourceProfile => Result[SourceProfile]],
    existence: Option[Existence]
  )

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.NonNullable("name", rName),
        SiderealInput.EditBinding.Option("sidereal", rSidereal),
        NonsiderealInput.EditBinding.Option("nonsidereal", rNonsidereal),
        OpportunityInput.EditBinding.Option("opportunity", rOpportunity),
        SourceProfileInput.EditBinding.Option("sourceProfile", rSourceProfile),
        ExistenceBinding.Option("existence", rExistence)
      ) =>
        val rSubtypeInfo =
          (rSidereal, rNonsidereal, rOpportunity).parFlatMapN: (s, n, o) =>
            atMostOne[SiderealInput.Edit | NonsiderealInput.Edit | OpportunityInput.Edit](
              s -> "sidereal",
              n -> "nonsidereal",
              o -> "opportunity"
            )
        (rName, rSubtypeInfo, rSourceProfile, rExistence).parMapN(Edit.apply)
    }

  val Binding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        SiderealInput.CreateBinding.Option("sidereal", rSidereal),
        NonsiderealInput.CreateBinding.Option("nonsidereal", rNonsidereal),
        OpportunityInput.CreateBinding.Option("opportunity", rOpportunity),
        SourceProfileInput.CreateBinding.Option("sourceProfile", rSourceProfile),
        ExistenceBinding.Option("existence", rExistence)
      ) =>
        val rNameʹ          = rName.flatMap(_.toResult("Target name is required on creation."))
        val rSourceProfileʹ = rSourceProfile.flatMap(_.toResult("Source Profile is required on creation."))
        val rSubtypeInfo    =
          (rSidereal, rNonsidereal, rOpportunity).parFlatMapN: (s, n, o) =>
            oneOrFail[SiderealInput.Create | NonsiderealInput.Create | OpportunityInput.Create](
              s -> "sidereal",
              n -> "nonsidereal",
              o -> "opportunity"
            )
        (rNameʹ, rSubtypeInfo, rSourceProfileʹ, rExistence).parMapN: (name, subtypeInfo, sourceProfile, existence) =>
          Create(name, subtypeInfo, sourceProfile, existence.getOrElse(Existence.Default))
    }
}
