// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.BrightnessUnits._
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.odb.graphql.binding._

object SpectralDefinitionInput {

  implicit class SpectralDefinitionProjections[A](self: SpectralDefinition[A]) {
    def bandNormalized = self match { case a: BandNormalized[A] => Result(a); case _ => Result.failure("Not a band normalized spectral definition.") }
    def emissionLines  = self match { case a: EmissionLines[A]  => Result(a); case _ => Result.failure("Not a emission lines spectral definition.") }
  }

  object Integrated {

    val CreateBinding: Matcher[SpectralDefinition[Integrated]] =
      createBinding(
        BandNormalizedInput.Integrated.CreateBinding,
        EmissionLinesInput.Integrated.CreateBinding,
      )

    val EditBinding: Matcher[SpectralDefinition[Integrated] => Result[SpectralDefinition[Integrated]]] =
      editBinding(
        BandNormalizedInput.Integrated.EditBinding,
        EmissionLinesInput.Integrated.EditBinding,
      )

  }

  object Surface {

    val CreateBinding: Matcher[SpectralDefinition[Surface]] =
      createBinding(
        BandNormalizedInput.Surface.CreateBinding,
        EmissionLinesInput.Surface.CreateBinding,
      )

    val EditBinding: Matcher[SpectralDefinition[Surface] => Result[SpectralDefinition[Surface]]] =
      editBinding(
        BandNormalizedInput.Surface.EditBinding,
        EmissionLinesInput.Surface.EditBinding,
      )

  }

  def createBinding[A](
    bandNormalized: Matcher[BandNormalized[A]],
    emissionLines: Matcher[EmissionLines[A]],
  ): Matcher[SpectralDefinition[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        bandNormalized.Option("bandNormalized", rBandNormalized),
        emissionLines.Option("emissionLines", rEmissionLines),
      ) =>
        (rBandNormalized, rEmissionLines).parTupled.flatMap {
          case (Some(bandNormalized), None) => Result(bandNormalized)
          case (None, Some(emissionLines))  => Result(emissionLines)
          case _                            => Result.failure("Expected exactly one of bandNormalized or emissionLines.")
        }
    }

  def editBinding[A](
    bandNormalized: Matcher[BandNormalized[A] => Result[BandNormalized[A]]],
    emissionLines: Matcher[EmissionLines[A] => EmissionLines[A]],
  ): Matcher[SpectralDefinition[A] => Result[SpectralDefinition[A]]] =
    ObjectFieldsBinding.rmap {
      case List(
        bandNormalized.Option("bandNormalized", rBandNormalized),
        emissionLines.Option("emissionLines", rEmissionLines),
      ) =>
        (rBandNormalized, rEmissionLines).parTupled.flatMap {
          case (Some(f), None) => Result(a => a.bandNormalized.flatMap(f))
          case (None, Some(f)) => Result(a => a.emissionLines.map(f))
          case _               => Result.failure("Expected exactly one of bandNormalized or emissionLines.")
        }
    }

}














