// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.SpectralDefinition

given [T]: Encoder[SpectralDefinition[T]] = {
  case bn @ SpectralDefinition.BandNormalized(_, _) =>
    Json.obj("bandNormalized" -> bn.asJson)

  case el @ SpectralDefinition.EmissionLines(_, _) =>
    Json.obj("emissionLines" -> el.asJson)
}
