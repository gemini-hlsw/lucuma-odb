package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.util.Bindings._

object FluxDensityInput {

  val Binding: Matcher[(Wavelength, PosBigDecimal)] =
    ObjectFieldsBinding.rmap {
      case List(
        WavelengthInput.Binding("wavelenth", rWavelength),
        BigDecimalBinding("density", rDensity)
      ) => (rWavelength, rDensity).parTupled.flatMap {
        case (wavelength, density) =>
          PosBigDecimal.from(density) match {
            case Left(err) => Result.failure(err)
            case Right(v)  => Result((wavelength, v))
          }
        }
      }
    }
