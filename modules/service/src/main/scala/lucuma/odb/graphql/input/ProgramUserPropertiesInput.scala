// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.model.PartnerLink
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*
import monocle.Focus
import monocle.Lens

case class ProgramUserPropertiesInput(
  partnerLink:       Option[PartnerLink],
  fallbackProfile:   Nullable[UserProfileInput],
  educationalStatus: Nullable[EducationalStatus],
  thesis:            Nullable[Boolean],
  gender:            Nullable[Gender],
  affiliation:       Nullable[NonEmptyString],
  hasDataAccess:     Option[Boolean]
)

object ProgramUserPropertiesInput:

  val Empty: ProgramUserPropertiesInput =
    ProgramUserPropertiesInput(
      none,
      Nullable.Absent,
      Nullable.Absent,
      Nullable.Absent,
      Nullable.Absent,
      Nullable.Absent,
      none
    )

  val partnerLink: Lens[ProgramUserPropertiesInput, Option[PartnerLink]] =
    Focus[ProgramUserPropertiesInput](_.partnerLink)

  val Binding: Matcher[ProgramUserPropertiesInput] =
    ObjectFieldsBinding.rmap:
      case List(
        PartnerLinkInput.Binding.Option("partnerLink", rPartnerLink),
        UserProfileInput.Binding.Nullable("fallbackProfile", rFallbackProfile),
        EducationalStatusBinding.Nullable("educationalStatus", rEducationalStatus),
        BooleanBinding.Nullable("thesis", rThesis),
        GenderBinding.Nullable("gender", rGender),
        NonEmptyStringBinding.Nullable("affiliation", rAffiliation),
        BooleanBinding.Option("hasDataAccess", rDataAccess)
      ) => (rPartnerLink, rFallbackProfile, rEducationalStatus, rThesis, rGender, rAffiliation, rDataAccess).parMapN(ProgramUserPropertiesInput.apply)
