// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan

case class SpectroscopyTimeAndGraphsResult(
  versions:       ItcVersions,
  targetOutcomes: AsterismTimesAndGraphsOutcomes
)

object SpectroscopyTimeAndGraphsResult:
  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[SpectroscopyTimeAndGraphsResult] =
    r =>
      Json
        .obj(
          "versions"             -> r.versions.asJson,
          "targetTimesAndGraphs" -> r.targetOutcomes.value.toOption.asJson,
          "targetTimes"          -> r.targetOutcomes.value.left.toOption.asJson,
          "brightestIndex"       -> r.targetOutcomes.value.toOption.flatMap(_.brightestIndex).asJson,
          "brightest"            -> r.targetOutcomes.value.toOption.flatMap(_.brightest).asJson
        )
