// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.ItcVersions
import lucuma.itc.encoders.given

case class CalculationResult(
  versions:         ItcVersions,
  mode:             ObservingMode,
  targetTimes:      AsterismIntegrationTimeOutcomes,
  exposureTimeMode: ExposureTimeMode
)

object CalculationResult:
  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[CalculationResult] = r =>
    Json
      .obj(
        "versions"         -> r.versions.asJson,
        "mode"             -> r.mode.asJson,
        "exposureTimeMode" -> r.exposureTimeMode.asJson,
        "targetTimes"      -> r.targetTimes.asJson,
        "brightestIndex"   -> r.targetTimes.brightestIndex.asJson,
        "brightest"        -> r.targetTimes.brightest.asJson
      )
