// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.client.json.encoders.given
import monocle.Focus
import monocle.Lens

case class SpectroscopyParameters(
  exposureTimeMode: ExposureTimeMode,
  constraints:      ItcConstraintsInput,
  mode:             InstrumentMode
) derives Eq

object SpectroscopyParameters:
  val exposureTimeMode: Lens[SpectroscopyParameters, ExposureTimeMode] =
    Focus[SpectroscopyParameters](_.exposureTimeMode)

  val constraints: Lens[SpectroscopyParameters, ItcConstraintsInput] =
    Focus[SpectroscopyParameters](_.constraints)

  val mode: Lens[SpectroscopyParameters, InstrumentMode] =
    Focus[SpectroscopyParameters](_.mode)

  given Encoder[SpectroscopyParameters] with
    def apply(a: SpectroscopyParameters): Json =
      Json
        .obj(
          "exposureTimeMode" -> a.exposureTimeMode.asJson,
          "constraints"      -> a.constraints.asJson,
          "mode"             -> a.mode.asJson
        )
        .dropNullValues

case class SpectroscopyInput(
  parameters: SpectroscopyParameters,
  asterism:   NonEmptyList[TargetInput]
) derives Eq:
  export parameters.*

object SpectroscopyInput:
  val parameters: Lens[SpectroscopyInput, SpectroscopyParameters] =
    Focus[SpectroscopyInput](_.parameters)

  val asterism: Lens[SpectroscopyInput, NonEmptyList[TargetInput]] =
    Focus[SpectroscopyInput](_.asterism)

  given Encoder[SpectroscopyInput] with
    def apply(a: SpectroscopyInput): Json =
      Json.obj("asterism" -> a.asterism.asJson).deepMerge(a.parameters.asJson)
