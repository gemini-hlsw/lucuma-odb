// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import grackle.Result
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.LineFluxValue
import lucuma.core.util.*
import lucuma.odb.graphql.binding.*

object LineFluxInput {
  object Integrated {
    val Binding: Matcher[LineFluxMeasure[Integrated]] =
      DecimalInput("LineFluxIntegrated") { case (bd, tag) =>
        LineFluxValue.from(bd) match {
          case Left(err)  => Result.failure(err)
          case Right(lfv) =>
            tag match {
              case "W_PER_M_SQUARED"          =>
                Result(WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(lfv))
              case "ERG_PER_S_PER_CM_SQUARED" =>
                Result(ErgsPerSecondCentimeter2IsIntegratedLineFluxUnit.unit.withValueTagged(lfv))
            }
        }
      }
  }
  object Surface    {
    val Binding: Matcher[LineFluxMeasure[Surface]] =
      DecimalInput("LineFluxSurface") { case (bd, tag) =>
        LineFluxValue.from(bd) match {
          case Left(err)  => Result.failure(err)
          case Right(lfv) =>
            tag match {
              case "W_PER_M_SQUARED_PER_ARCSEC_SQUARED"          =>
                Result(WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(lfv))
              case "ERG_PER_S_PER_CM_SQUARED_PER_ARCSEC_SQUARED" =>
                Result(
                  ErgsPerSecondCentimeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(lfv)
                )
            }
        }
      }
  }
}
