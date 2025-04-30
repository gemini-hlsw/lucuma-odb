// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Codec
import io.circe.Decoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.util.CalculationState
import lucuma.core.util.CalculatedValue

trait CalculatedValueCodec:

  given [A: Codec]: Codec[CalculatedValue[A]] with
    def apply(c: CalculatedValue[A]): Json =
      Json.obj(
        "state" -> c.state.asJson,
        "value" -> c.value.asJson
      )

    def apply(c: HCursor): Decoder.Result[CalculatedValue[A]] =
      for
        p <- c.downField("state").as[CalculationState]
        v <- c.downField("value").as[A]
      yield CalculatedValue(p, v)

object calculationValue extends CalculatedValueCodec

