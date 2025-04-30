// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Codec
import io.circe.Decoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.util.CalculationPhase
import lucuma.core.util.CalculationValue

trait CalculationValueCodec:

  given [A: Codec]: Codec[CalculationValue[A]] with
    def apply(c: CalculationValue[A]): Json =
      Json.obj(
        "phase" -> c.phase.asJson,
        "value" -> c.value.asJson
      )

    def apply(c: HCursor): Decoder.Result[CalculationValue[A]] =
      for
        p <- c.downField("phase").as[CalculationPhase]
        v <- c.downField("value").as[A]
      yield CalculationValue(p, v)

object calculationValue extends CalculationValueCodec

