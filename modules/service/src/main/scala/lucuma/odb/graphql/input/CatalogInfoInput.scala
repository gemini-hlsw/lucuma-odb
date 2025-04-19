// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CatalogName
import lucuma.odb.graphql.binding.*

final case class CatalogInfoInput(
  name: Option[CatalogName],
  id: Option[NonEmptyString],
  objectType: Option[NonEmptyString],
)

object CatalogInfoInput {

  val Empty = CatalogInfoInput(None, None, None)

  val CatalogNameBinding = enumeratedBinding[CatalogName]

  val Binding = ObjectFieldsBinding.rmap {
    case List(
      CatalogNameBinding.Option("name", rName),
      NonEmptyStringBinding.Option("id", rId),
      NonEmptyStringBinding.Option("objectType", rObjectType)
    ) =>
      (rName, rId, rObjectType).parMapN(CatalogInfoInput.apply)
  }

}