// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.Of
import lucuma.core.math.dimensional.Units
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._

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