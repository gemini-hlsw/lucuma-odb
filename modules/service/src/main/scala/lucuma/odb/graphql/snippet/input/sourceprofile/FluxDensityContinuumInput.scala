package lucuma.odb.graphql.snippet
package input
package sourceprofile

import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.Of
import lucuma.odb.graphql.util.Bindings._
import cats.syntax.all._
import lucuma.core.math.dimensional.Units
import edu.gemini.grackle.Result

object FluxDensityContinuumInput {

  object Integrated {
    val Binding: Matcher[Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]] =
      binding(enumeratedBinding[Units Of FluxDensityContinuum[Integrated]])
  }

  object Surface {
    val Binding: Matcher[Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]] =
      binding(enumeratedBinding[Units Of FluxDensityContinuum[Surface]])
  }

  def binding[A](
    unitsBinding: Matcher[Units Of FluxDensityContinuum[A]]
  ): Matcher[Measure[PosBigDecimal] Of FluxDensityContinuum[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        BigDecimalBinding("value", rValue),
        unitsBinding("units", rUnits),
      ) =>
        (rValue, rUnits).parTupled.flatMap {
          case (value, units) =>
            PosBigDecimal.from(value) match {
              case Left(err) => Result.failure(err)
              case Right(value) => Result(units.withValueTagged(value))
            }
        }
    }

}