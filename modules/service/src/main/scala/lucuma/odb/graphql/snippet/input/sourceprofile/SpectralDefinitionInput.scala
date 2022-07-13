package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.BrightnessUnits._
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.odb.graphql.util.Bindings._

object SpectralDefinitionInput {

  object Integrated {
    val CreateBinding: Matcher[SpectralDefinition[Integrated]] =
      createBinding(
        BandNormalizedInput.Integrated.CreateBinding,
        EmissionLinesInput.Integrated.CreateBinding,
      )
  }

  object Surface {
    val CreateBinding: Matcher[SpectralDefinition[Surface]] =
      createBinding(
        BandNormalizedInput.Surface.CreateBinding,
        EmissionLinesInput.Surface.CreateBinding,
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

}














