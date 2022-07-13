package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.kernel.Order
import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.Of
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.odb.graphql.util.Bindings._

import scala.collection.immutable.TreeMap

object EmissionLinesInput {

  implicit val WavelengthOrdering: Ordering[Wavelength] =
    Order[Wavelength].toOrdering

  object Integrated {
    val CreateBinding = createBinding[Integrated](
      EmissionLineInput.Integrated.CreateBinding,
      FluxDensityContinuumInput.Integrated.Binding,
    )
  }

  object Surface {
    val CreateBinding = createBinding[Surface](
      EmissionLineInput.Surface.CreateBinding,
      FluxDensityContinuumInput.Surface.Binding,
    )
  }

  def createBinding[A](
    line: Matcher[(Wavelength, EmissionLine[A])],
    fluxDensityContinuum:  Matcher[Measure[PosBigDecimal] Of FluxDensityContinuum[A]],
  ): Matcher[EmissionLines[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        line.List.Option("lines", rLines),
        line.List.Option("editLines", rEditLines),
        WavelengthInput.Binding.List.Option("deleteLines", rDeleteLines),
        fluxDensityContinuum.Option("fluxDensityContinuum", rFluxDensityContinuum),
      ) =>
        (rLines, rEditLines, rDeleteLines, rFluxDensityContinuum).parTupled.flatMap {
          case (Some(lines), None, None, Some(fluxDensityContinuum)) => Result(EmissionLines(lines.to(TreeMap), fluxDensityContinuum))
          case (Some(lines), _, _, Some(fluxDensityContinuum))       => Result.warning("editLines and deleteLines are ignored on creation.", EmissionLines(lines.to(TreeMap), fluxDensityContinuum))
          case _                                                     => Result.failure("Both lines and fluxDensityContinuum are required on creation.")
        }
    }

}