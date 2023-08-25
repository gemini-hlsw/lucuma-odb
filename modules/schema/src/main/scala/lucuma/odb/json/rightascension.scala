// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.RightAscension

object rightascension {

  trait QueryEncoder {

    given Encoder[RightAscension] =
      Encoder.instance { ra =>
        Json.obj(
          "hms"             -> RightAscension.fromStringHMS.reverseGet(ra).asJson,
          "hours"           -> BigDecimal(ra.toHourAngle.toDoubleHours).asJson,
          "degrees"         -> BigDecimal(ra.toAngle.toDoubleDegrees).asJson,
          "microarcseconds" -> ra.toAngle.toMicroarcseconds.asJson,
          "microseconds"    -> ra.toHourAngle.toMicroseconds.asJson
        )
      }
  }

  object query extends QueryEncoder
}
