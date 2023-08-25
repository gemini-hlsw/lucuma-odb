// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Declination

object declination {

  trait QueryEncoder {

    given Encoder[Declination] =
      Encoder.instance { dec =>
        Json.obj(
          "dms"             -> Declination.fromStringSignedDMS.reverseGet(dec).asJson,
          "degrees"         -> BigDecimal(dec.toAngle.toDoubleDegrees).asJson,
          "microarcseconds" -> dec.toAngle.toMicroarcseconds.asJson
        )
      }
  }

  object query extends QueryEncoder
}
