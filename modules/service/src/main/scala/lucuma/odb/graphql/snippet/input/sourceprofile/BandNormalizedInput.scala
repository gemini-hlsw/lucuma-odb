package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.odb.graphql.util.Bindings._

import scala.collection.immutable.SortedMap

object BandNormalizedInput {

  /** Binding[(K, V)] to Binding[SortedMap[K, V]] */
  def pairToMap[K: Ordering, V](pair: Matcher[(K, V)]): Matcher[SortedMap[K, V]] =
    pair.List.map(SortedMap.from(_))

  object Integrated {
    val CreateBinding =
      createBinding(pairToMap(BandBrightnessInput.Integrated.CreateBinding))
  }

  object Surface {
    val CreateBinding =
      createBinding(pairToMap(BandBrightnessInput.Surface.CreateBinding))
  }

  def createBinding[A](
    brightnesses: Matcher[SortedMap[Band, BrightnessMeasure[A]]]
  ): Matcher[BandNormalized[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        UnnormalizedSedInput.Binding.Option("sed", rSed),
        brightnesses.Option("brightnesses", rBrightnesses),
        brightnesses.Option("editBrightnesses", rEditBrightnesses),
        BandBinding.List.Option("deleteBrightnesses", rDeleteBrightnesses),
      ) =>
        (rSed, rBrightnesses, rEditBrightnesses, rDeleteBrightnesses).parTupled.flatMap {
          case (Some(sed), Some(brightnesses), None, None) => Result(BandNormalized(sed, brightnesses))
          case (Some(sed), Some(brightnesses), _, _)       => Result.warning("editBrightness and deleteBrightness are ignored on creation.", BandNormalized(sed, brightnesses))
          case _                                           => Result.failure("Both sed and brightness are required.")
        }
    }

}
