// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.odb.data.Nullable
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

object ProposalPropertiesInput {

  case class Create(
    title:    Option[NonEmptyString],
    category: Option[Tag],
    abstrakt: Option[NonEmptyString],
    call:     CallPropertiesInput.Create
  )

  case class Edit(
    title:    Nullable[NonEmptyString],
    category: Nullable[Tag],
    abstrakt: Nullable[NonEmptyString],
    call:     Option[CallPropertiesInput.Edit]
  )

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("title", rTitle),
        TagBinding.Option("category", rCategory),
        NonEmptyStringBinding.Option("abstract", rAbstract),
        CallPropertiesInput.Create.Binding.Option("callProperties", rCall)
      ) =>
        val c = rCall.map(_.getOrElse(CallPropertiesInput.Create.Default))
        (rTitle, rCategory, rAbstract, c).parMapN(Create.apply)
    }

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("title", rTitle),
        TagBinding.Nullable("category", rCategory),
        NonEmptyStringBinding.Nullable("abstract", rAbstract),
        CallPropertiesInput.Edit.Binding.Option("callProperties", rCall)
      ) =>
        (rTitle, rCategory, rAbstract, rCall).parMapN(Edit.apply)
    }

}

