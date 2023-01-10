// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.odb.graphql.binding._

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
      ) =>
        (rSed, rBrightnesses).parTupled.flatMap {
          case (sed, Some(brightnesses)) => Result(BandNormalized(sed, brightnesses))
          case _                         => Result.failure("Brightness is required.")
        }
    }

  def editBinding[A](
    brightnesses: Matcher[SortedMap[Band, BrightnessMeasure[A]]]
  ): Matcher[BandNormalized[A] => Result[BandNormalized[A]]] =
    ObjectFieldsBinding.rmap {
      case List(
        UnnormalizedSedInput.Binding.Nullable("sed", rSed),
        brightnesses.Option("brightnesses", rBrightnesses),
      ) =>
        (rSed, rBrightnesses).parTupled.flatMap {
          case (sed, brightnesses) =>
            Result { a0 =>
              val a1 = sed.fold(a0.copy(sed = none), a0, b => a0.copy(sed = b.some))
              val a2 = brightnesses.foldLeft(a1)((a, b) => a.copy(brightnesses = b))
              Result(a2)
            }
        }
    }
}
