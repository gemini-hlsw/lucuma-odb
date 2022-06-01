package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._

final case class NonsiderealInput()
object NonsiderealInput {
  val Binding: Matcher[NonsiderealInput] =
    ObjectFieldsBinding.emap {
      case List(

      ) => Left("no")
    }
}

