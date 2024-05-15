// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.model.CallForProposals
import lucuma.odb.data.Nullable
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

object ProposalPropertiesInput {

  case class Create(
    title:     Option[NonEmptyString],
    category:  Option[Tag],
    abstractʹ: Option[NonEmptyString],
    callId:    Option[CallForProposals.Id],
    typeʹ:     ProposalTypeInput.Create
  )

  case class Edit(
    title:     Nullable[NonEmptyString],
    category:  Nullable[Tag],
    abstractʹ: Nullable[NonEmptyString],
    callId:    Nullable[CallForProposals.Id],
    typeʹ:     Option[ProposalTypeInput.Edit]
  )

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("title", rTitle),
        TagBinding.Option("category", rCategory),
        NonEmptyStringBinding.Option("abstract", rAbstract),
        CallForProposalsIdBinding.Option("callId", rCid),
        ProposalTypeInput.Create.Binding.Option("type", rType)
      ) =>
        val t = rType.map(_.getOrElse(ProposalTypeInput.Create.Default))
        (rTitle, rCategory, rAbstract, rCid, t).parMapN(Create.apply)
    }

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("title", rTitle),
        TagBinding.Nullable("category", rCategory),
        NonEmptyStringBinding.Nullable("abstract", rAbstract),
        CallForProposalsIdBinding.Nullable("callId", rCid),
        ProposalTypeInput.Edit.Binding.Option("type", rType)
      ) =>
        (rTitle, rCategory, rAbstract, rCid, rType).parMapN(Edit.apply)
    }

}

