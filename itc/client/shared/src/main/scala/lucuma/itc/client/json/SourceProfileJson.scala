// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.SourceProfile

given Encoder[SourceProfile] = {
  case SourceProfile.Point(s) =>
    Json.obj("point" -> s.asJson)

  case SourceProfile.Uniform(s) =>
    Json.obj("uniform" -> s.asJson)

  case SourceProfile.Gaussian(f, s) =>
    Json.obj(
      "gaussian" -> Json.obj(
        "fwhm"               -> Json.obj("microarcseconds" -> f.toMicroarcseconds.asJson),
        "spectralDefinition" -> s.asJson
      )
    )
}
