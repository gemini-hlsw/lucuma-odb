package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.enum.EphemerisKeyType
import eu.timepit.refined.types.string.NonEmptyString
import cats.syntax.all._

final case class NonsiderealInput(
  keyType: Option[EphemerisKeyType],
  des: Option[NonEmptyString],
  key: Option[NonEmptyString],
)

object NonsiderealInput {

  val EphemerisKeyTypeBinding = enumeratedBinding[EphemerisKeyType]

  val Binding: Matcher[NonsiderealInput] =
    ObjectFieldsBinding.rmap {
      case List(
        EphemerisKeyTypeBinding.Option("keyType", rKeyType),
        NonEmptyStringBinding.Option("des", rDes),
        NonEmptyStringBinding.Option("key", rKey)
      ) =>
        (rKeyType, rDes, rKey).parMapN(NonsiderealInput.apply)
    }
}

