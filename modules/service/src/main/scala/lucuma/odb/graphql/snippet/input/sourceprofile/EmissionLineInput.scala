package lucuma.odb.graphql.snippet.input.sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.Of
import lucuma.core.model.EmissionLine
import lucuma.odb.graphql.util.Bindings._

object EmissionLineInput {

  object Integrated {
    val CreateBinding = createBinding[Integrated](LineFluxInput.Integrated.Binding)
  }

  object Surface {
    val CreateBinding = createBinding[Surface](LineFluxInput.Surface.Binding)
  }

  def createBinding[A](
    lineFlux: Matcher[Measure[PosBigDecimal] Of LineFlux[A]]
  ): Matcher[(Wavelength, EmissionLine[A])] =
    ObjectFieldsBinding.rmap {
      case List(
        WavelengthInput.Binding("wavelength", rWavelength),
        LineWidthInput.Binding.Option("lineWidth", rLineWidth),
        lineFlux.Option("lineFlux", rLineFlux),
      ) =>
        (rWavelength, rLineWidth, rLineFlux).parTupled.flatMap {
          case (wavelength, Some(lineWidth), Some(lineFlux)) => Result((wavelength, EmissionLine(lineWidth, lineFlux)))
          case _ => Result.failure("All fields are required on creation.")
        }
    }

}