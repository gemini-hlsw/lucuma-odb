// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.encoders

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan

given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[ExposureTimeMode] = {
  case ExposureTimeMode.SignalToNoiseMode(n, w) =>
    Json.obj(
      "signalToNoise" -> Json.obj(
        "value" -> n.asJson,
        "at"    -> w.asJson
      )
    )

  case ExposureTimeMode.TimeAndCountMode(t, c, w) =>
    Json.obj(
      "timeAndCount" -> Json.obj(
        "time"  -> t.asJson,
        "count" -> c.value.asJson,
        "at"    -> w.asJson
      )
    )
}
