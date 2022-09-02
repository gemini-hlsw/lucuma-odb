// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.Of
import lucuma.odb.graphql.binding._

object LineFluxInput {
  object Integrated {
    val Binding: Matcher[Measure[PosBigDecimal] Of LineFlux[Integrated]] =
      DecimalInput("LineFluxIntegrated") {
        case (bd, tag) =>
          PosBigDecimal.from(bd) match {
            case Left(err) => Result.failure(err)
            case Right(pbd) =>
              tag match {
                case "W_PER_M_SQUARED" =>
                  Result(WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(pbd))
                case "ERG_PER_S_PER_CM_SQUARED" =>
                  Result(ErgsPerSecondCentimeter2IsIntegratedLineFluxUnit.unit.withValueTagged(pbd))
              }
          }
      }
  }
  object Surface {
    val Binding: Matcher[Measure[PosBigDecimal] Of LineFlux[Surface]] =
      DecimalInput("LineFluxSurface") {
        case (bd, tag) =>
          PosBigDecimal.from(bd) match {
            case Left(err) => Result.failure(err)
            case Right(pbd) =>
              tag match {
                case "W_PER_M_SQUARED_PER_ARCSEC_SQUARED" =>
                  Result(WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(pbd))
                case "ERG_PER_S_PER_CM_SQUARED_PER_ARCSEC_SQUARED" =>
                  Result(ErgsPerSecondCentimeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(pbd))
              }
          }
      }
  }
}