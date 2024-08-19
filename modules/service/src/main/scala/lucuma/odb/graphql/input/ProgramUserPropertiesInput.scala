// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.PartnerLink
import lucuma.odb.graphql.binding.*

case class ProgramUserPropertiesInput(
  partnerLink: Option[PartnerLink]
)

object ProgramUserPropertiesInput {

  val Binding: Matcher[ProgramUserPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        PartnerLinkInput.Binding.Option("partnerLink", rPartnerLink)
      ) => rPartnerLink.map(ProgramUserPropertiesInput.apply)
    }

}