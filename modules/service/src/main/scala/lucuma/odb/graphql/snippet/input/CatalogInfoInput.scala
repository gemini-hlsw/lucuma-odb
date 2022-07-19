package lucuma.odb.graphql.snippet
package input

import cats.syntax.all._
import lucuma.odb.graphql.util.Bindings._
import lucuma.core.enum.CatalogName
import eu.timepit.refined.types.string.NonEmptyString

final case class CatalogInfoInput(
  name: Option[CatalogName],
  id: Option[NonEmptyString],
  objectType: Option[NonEmptyString],
)

object CatalogInfoInput {

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