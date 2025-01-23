// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.CallForProposals
import lucuma.odb.data.Nullable
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

object ProposalPropertiesInput {

  case class Create(
    category:  Option[Tag],
    callId:    Option[CallForProposals.Id],
    typeʹ:     ProposalTypeInput.Create
  )

  case class Edit(
    category:  Nullable[Tag],
    callId:    Nullable[CallForProposals.Id],
    typeʹ:     Option[ProposalTypeInput.Edit]
  )

  object Edit {
    val Empty: Edit =
      Edit(Nullable.Absent, Nullable.Absent, None)
  }

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        TagBinding.Option("category", rCategory),
        CallForProposalsIdBinding.Option("callId", rCid),
        ProposalTypeInput.Create.Binding.Option("type", rType)
      ) =>
        val t = rType.map(_.getOrElse(ProposalTypeInput.Create.Default))
        (rCategory, rCid, t).parMapN(Create.apply)
    }

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        TagBinding.Nullable("category", rCategory),
        CallForProposalsIdBinding.Nullable("callId", rCid),
        ProposalTypeInput.Edit.Binding.Option("type", rType)
      ) =>
        (rCategory, rCid, rType).parMapN(Edit.apply)
    }

}

