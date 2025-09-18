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

case class ImagingParameters(
  exposureTimeMode: ExposureTimeMode,
  constraints:      ItcConstraintsInput,
  mode:             InstrumentMode
) derives Eq

object ImagingParameters {
  given Encoder[ImagingParameters] with
    def apply(a: ImagingParameters): Json =
      Json
        .obj(
          "exposureTimeMode" -> a.exposureTimeMode.asJson,
          "constraints"      -> a.constraints.asJson,
          "mode"             -> a.mode.asJson
        )
        .dropNullValues

  val exposureTimeMode: Lens[ImagingParameters, ExposureTimeMode] =
    Focus[ImagingParameters](_.exposureTimeMode)

  val constraints: Lens[ImagingParameters, ItcConstraintsInput] =
    Focus[ImagingParameters](_.constraints)

  val mode: Lens[ImagingParameters, InstrumentMode] =
    Focus[ImagingParameters](_.mode)
}

final case class ImagingInput(
  parameters: ImagingParameters,
  asterism:   NonEmptyList[TargetInput]
) derives Eq:
  export parameters.*

object ImagingInput {
  val parameters: Lens[ImagingInput, ImagingParameters] =
    Focus[ImagingInput](_.parameters)

  val asterism: Lens[ImagingInput, NonEmptyList[TargetInput]] =
    Focus[ImagingInput](_.asterism)

  given Encoder[ImagingInput] with
    def apply(a: ImagingInput): Json =
      Json.obj("asterism" -> a.asterism.asJson).deepMerge(a.parameters.asJson)
}
