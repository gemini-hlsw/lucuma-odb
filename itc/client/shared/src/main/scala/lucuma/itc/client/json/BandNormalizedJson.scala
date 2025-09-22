// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.model.SpectralDefinition.BandNormalized

import syntax.*

given [T]: Encoder[BandNormalized[T]] with
  def apply(bn: BandNormalized[T]): Json =
    Json.obj(
      "sed"          -> bn.sed.asJson,
      "brightnesses" -> Json.arr(bn.brightnesses.toList.map { case (b, m) =>
        Json.fromFields(
          List(
            "band"  -> b.asScreamingJson,
            "value" -> m.value.asJson,
            "units" -> m.units.serialized.asJson
          ) ++ m.error.toList.map(v => "error" -> v.asJson)
        )
      }*)
    )
