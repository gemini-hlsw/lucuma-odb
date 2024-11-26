// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.model.PartnerLink
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class ProgramUserPropertiesInput(
  partnerLink:       Option[PartnerLink],
  fallbackProfile:   Nullable[UserProfileInput],
  educationalStatus: Nullable[EducationalStatus],
  thesis:            Nullable[Boolean],
  gender:            Nullable[Gender],
)

object ProgramUserPropertiesInput:
  val Binding: Matcher[ProgramUserPropertiesInput] =
    ObjectFieldsBinding.rmap:
      case List(
        PartnerLinkInput.Binding.Option("partnerLink", rPartnerLink),
        UserProfileInput.Binding.Nullable("fallbackProfile", rFallbackProfile),
        EducationalStatusBinding.Nullable("educationalStatus", rEducationalStatus),
        BooleanBinding.Nullable("thesis", rThesis),
        GenderBinding.Nullable("gender", rGender),
      ) => (rPartnerLink, rFallbackProfile, rEducationalStatus, rThesis, rGender).parMapN(ProgramUserPropertiesInput.apply)