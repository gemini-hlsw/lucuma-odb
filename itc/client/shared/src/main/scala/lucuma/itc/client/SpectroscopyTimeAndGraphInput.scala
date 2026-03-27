// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import io.circe.Encoder
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.core.util.TimeSpan

case class SpectroscopyIntegrationTimeAndGraphsParameters(
  constraints:        ItcConstraintsInput,
  mode:               InstrumentMode,
  significantFigures: Option[SignificantFiguresInput]
) derives Eq

object SpectroscopyIntegrationTimeAndGraphsParameters {
  given Encoder.AsObject[SpectroscopyIntegrationTimeAndGraphsParameters] = a =>
    JsonObject(
      "constraints"        -> a.constraints.asJson,
      "mode"               -> a.mode.asJson,
      "significantFigures" -> a.significantFigures.asJson
    )
}

case class SpectroscopyIntegrationTimeAndGraphsInput(
  parameters: SpectroscopyIntegrationTimeAndGraphsParameters,
  asterism:   NonEmptyList[TargetInput]
) derives Eq:
  export parameters.*

object SpectroscopyIntegrationTimeAndGraphsInput {
  given Encoder[TimeSpan] = _.toMicroseconds.asJson

  given Encoder.AsObject[SpectroscopyIntegrationTimeAndGraphsInput] = a =>
    JsonObject("asterism" -> a.asterism.asJson).deepMerge(a.parameters.asJsonObject)

}
