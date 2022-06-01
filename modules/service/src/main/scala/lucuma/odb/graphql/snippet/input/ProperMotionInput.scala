package lucuma.odb.graphql.snippet
package input

import cats.syntax.all._
import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.ProperMotion

object ProperMotionInput {

  val Binding: Matcher[ProperMotion] =
    ObjectFieldsBinding.rmap {
      case List(
        ProperMotionComponentInput.RA.Binding("ra", rRa),
        ProperMotionComponentInput.Dec.Binding("dec", rDec)
      ) =>
        (rRa, rDec).mapN(ProperMotion.apply)
    }

}