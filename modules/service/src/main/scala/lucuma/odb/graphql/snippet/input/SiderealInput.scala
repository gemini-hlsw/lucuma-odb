package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import edu.gemini.grackle.Result
import cats.syntax.all._
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Parallax
import lucuma.core.model.CatalogInfo

final case class SiderealInput(
  ra: Option[RightAscension],
  dec: Option[Declination],
  epoch: Option[Epoch],
  properMotion: Option[ProperMotion],
  radialVelocity: Option[RadialVelocity],
  parallax: Option[Parallax],
  catalogInfo: Option[CatalogInfoInput],
)

object SiderealInput {
  val Binding: Matcher[SiderealInput] =
    ObjectFieldsBinding.rmap {
      case List(
        RightAscensionInput.Binding.Option("ra", rRa),
        DeclinationInput.Binding.Option("dec", rDec),
        EpochBinding.Option("epoch", rEpoch),
        ProperMotionInput.Binding.Option("properMotion", rProperMotion),
        RadialVelocityInput.Binding.Option("radialVelocity", rRadialVelocity),
        ParallaxModelInput.Binding.Option("parallax", rParallax),
        CatalogInfoInput.Binding.Option("catalogInfo", rCatalogInfo)
      ) =>
        (rRa, rDec, rEpoch, rProperMotion, rRadialVelocity, rParallax, rCatalogInfo).parMapN {
          SiderealInput(_, _, _, _, _, _, _)
        }
    }
}


