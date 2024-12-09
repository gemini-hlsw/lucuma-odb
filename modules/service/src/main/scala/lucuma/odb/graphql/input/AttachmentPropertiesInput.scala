// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object AttachmentPropertiesInput {

  case class Edit(
    description: Nullable[NonEmptyString],
    checked: Option[Boolean]
  )

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("description", rDescription),
        BooleanBinding.Option("checked", rChecked)
      ) =>
        (rDescription, rChecked).parMapN(Edit.apply)
    }
}
