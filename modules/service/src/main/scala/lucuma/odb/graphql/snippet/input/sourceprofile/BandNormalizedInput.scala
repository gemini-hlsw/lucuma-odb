// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.odb.graphql.util.Bindings._

import scala.collection.immutable.SortedMap

object BandNormalizedInput {

  /** Binding[(K, V)] to Binding[SortedMap[K, V]] */
  def pairToMap[K: Ordering, V](pair: Matcher[(K, V)]): Matcher[SortedMap[K, V]] =
    pair.List.map(SortedMap.from(_))

  object Integrated {

    val CreateBinding: Matcher[BandNormalized[Integrated]] =
      createBinding(pairToMap(BandBrightnessInput.Integrated.Binding))

    val EditBinding: Matcher[BandNormalized[Integrated] => Result[BandNormalized[Integrated]]] =
      editBinding(pairToMap(BandBrightnessInput.Integrated.Binding))

  }

  object Surface {

    val CreateBinding: Matcher[BandNormalized[Surface]] =
      createBinding(pairToMap(BandBrightnessInput.Surface.Binding))

    val EditBinding: Matcher[BandNormalized[Surface] => Result[BandNormalized[Surface]]] =
      editBinding(pairToMap(BandBrightnessInput.Surface.Binding))

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

  def editBinding[A](
    brightnesses: Matcher[SortedMap[Band, BrightnessMeasure[A]]]
    ): Matcher[BandNormalized[A] => Result[BandNormalized[A]]] =
    ObjectFieldsBinding.rmap {
      case List(
        UnnormalizedSedInput.Binding.Option("sed", rSed),
        brightnesses.Option("brightnesses", rBrightnesses),
        brightnesses.Option("editBrightnesses", rEditBrightnesses),
        BandBinding.List.Option("deleteBrightnesses", rDeleteBrightnesses),
      ) =>
        (rSed, rBrightnesses, rEditBrightnesses, rDeleteBrightnesses).parTupled.flatMap {
          case (sed, brightnesses, editBrightness, deleteBrightness) =>
            Result { a0 =>
              val a1 = sed.foldLeft(a0)((a, b) => a.copy(sed = b))
              val a2 = brightnesses.foldLeft(a1)((a, b) => a.copy(brightnesses = b))
              val a3 = editBrightness.foldLeft(a2)((a, b) => a.copy(brightnesses = a.brightnesses ++ b))
              val a4 = deleteBrightness.foldLeft(a3)((a, b) => a.copy(brightnesses = a.brightnesses -- b))
              Result(a4)
            }
        }
    }
}
