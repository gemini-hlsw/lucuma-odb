// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.RadialVelocity

object radialvelocity {

  trait QueryEncoder {

    given Encoder[RadialVelocity] =
      Encoder.instance { rv =>
        Json.obj(
          "metersPerSecond"      -> rv.rv.value.asJson,
          "kilometersPerSecond"  -> (rv.rv.value / BigDecimal(1000)).asJson,
          "centimetersPerSecond" -> (rv.rv.value * BigDecimal(100)).toLong.asJson
        )
      }
  }

  object query extends QueryEncoder
}
