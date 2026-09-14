// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.*
import io.circe.syntax.*
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.itc.AltairParameters
import lucuma.itc.client.*
import lucuma.itc.client.json.syntax.*

// Decoders for the client don't need to be as generic as the ones for the server.
private[client] object encoders:
  // TODO get this directly from the odb. Also other encoders that are duplicated here.
  given Encoder[TimeSpan] = t =>
    Json.obj("microseconds" -> TimeSpan.FromMicroseconds.reverseGet(t).asJson)

  given (using Encoder[Wavelength]): Encoder[ExposureTimeMode.TimeAndCountMode] = t =>
    Json.obj(
      "time"  -> t.time.asJson,
      "count" -> t.count.value.asJson,
      "at"    -> t.at.asJson
    )

  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[ExposureTimeMode] = {
    case ExposureTimeMode.SignalToNoiseMode(n, w) =>
      Json.obj(
        "signalToNoise" -> Json.obj(
          "value" -> n.asJson,
          "at"    -> w.asJson
        )
      )

    case ExposureTimeMode.TimeAndCountMode(t, c, w) => // TODO add coadds
      Json.obj(
        "timeAndCount" -> Json.obj(
          "time"  -> t.asJson,
          "count" -> c.value.asJson,
          "at"    -> w.asJson
        )
      )
  }

  private def arcsecJson(x: Angle): Json =
    Json.obj("arcseconds" -> Angle.signedDecimalArcseconds.get(x).asJson)

  // The GraphQL `AltairInput`: a mode plus the fields that mode needs.
  given Encoder[AltairParameters] = Encoder.instance:
    case AltairParameters.Ngs(separation, brightness, fieldLens) =>
      Json.obj(
        "mode"                -> Json.fromString("NGS"),
        "guideStarSeparation" -> arcsecJson(separation),
        "guideStarBrightness" -> brightness.value.value.asJson,
        "fieldLens"           -> fieldLens.asScreamingJson
      )
    case AltairParameters.Lgs(separation, brightness)            =>
      Json.obj(
        "mode"                -> Json.fromString("LGS"),
        "guideStarSeparation" -> arcsecJson(separation),
        "guideStarBrightness" -> brightness.value.value.asJson
      )
    case AltairParameters.LgsP1                                  =>
      Json.obj("mode" -> Json.fromString("LGS_P1"))
