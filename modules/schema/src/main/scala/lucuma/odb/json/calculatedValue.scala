// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState

trait CalculatedValueCodec:

  given [A: Encoder]: Encoder[CalculatedValue[A]] with
    def apply(c: CalculatedValue[A]): Json =
      Json.obj(
        "calculationState" -> c.state.asJson,
        "state"            -> c.state.asJson,
        "value"            -> c.value.asJson
      )

  given [A: Decoder]: Decoder[CalculatedValue[A]] with
    def apply(c: HCursor): Decoder.Result[CalculatedValue[A]] =
      for
        p <- c.downField("calculationState").as[CalculationState]
             .orElse(c.downField("state").as[CalculationState])
        v <- c.downField("value").as[A]
      yield CalculatedValue(p, v)

object calculatedValue extends CalculatedValueCodec
