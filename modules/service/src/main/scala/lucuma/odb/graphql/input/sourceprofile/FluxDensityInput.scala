// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.binding._

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
