// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import grackle.Result
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.binding.*

object FluxDensityInput {

  val Binding: Matcher[(Wavelength, PosBigDecimal)] =
    ObjectFieldsBinding.rmap {
      case List(
        WavelengthInput.Binding("wavelenth", rWavelength),
        BigDecimalBinding("density", rDensity)
      ) => (rWavelength, rDensity).parTupled.flatMap {
        case (wavelength, density) =>
          PosBigDecimal.from(density) match {
            case Left(err) => Matcher.validationFailure(err)
            case Right(v)  => Result((wavelength, v))
          }
        }
      }
    }
