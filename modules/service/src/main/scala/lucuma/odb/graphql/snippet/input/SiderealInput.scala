package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import edu.gemini.grackle.Result

final case class SiderealInput(
  ra: RightAscension,
  dec: Declination,
  epoch: Epoch,
)

object SiderealInput {
  val Binding: Matcher[SiderealInput] =
    ObjectFieldsBinding.rmap {
      case List(
        RightAscensionInput.Binding("ra", rRa),
        DeclinationInput.Binding("dec", rDec),
        EpochBinding("epoch", rEpoch),
        ProperMotionInput.Binding("properMotion", rProperMotion),
        RadialVelocityInput.Binding("radialVelocity", rRadialVelocity),
        // parallax
        // catalogInfo
      ) => Result.failure("no")
    }
}


