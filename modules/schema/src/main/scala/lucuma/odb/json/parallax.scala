// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Parallax

import java.math.MathContext

object parallax {

  trait QueryEncoder {

    given Encoder[Parallax] =
      Encoder.instance { p =>
        Json.obj(
          "microarcseconds" -> p.μas.value.value.asJson,
          "milliarcseconds" -> p.mas.value.toBigDecimal(MathContext.DECIMAL128).asJson
        )
      }
  }

  object query extends QueryEncoder
}
