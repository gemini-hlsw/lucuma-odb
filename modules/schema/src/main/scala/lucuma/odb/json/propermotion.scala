// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.ProperMotion

object propermotion {

  trait QueryEncoder {

    given Encoder[ProperMotion.RA] =
      Encoder.instance { ra =>
        Json.obj(
          "microarcsecondsPerYear" -> ProperMotion.RA.microarcsecondsPerYear.reverseGet(ra).asJson,
          "milliarcsecondsPerYear" -> ProperMotion.RA.milliarcsecondsPerYear.get(ra).asJson
        )
      }

    given Encoder[ProperMotion.Dec] =
      Encoder.instance { dec =>
        Json.obj(
          "microarcsecondsPerYear" -> ProperMotion.Dec.microarcsecondsPerYear.reverseGet(dec).asJson,
          "milliarcsecondsPerYear" -> ProperMotion.Dec.milliarcsecondsPerYear.get(dec).asJson
        )
      }

    given Encoder[ProperMotion] =
      Encoder.instance { pm =>
        Json.obj(
          "ra"  -> pm.ra.asJson,
          "dec" -> pm.dec.asJson
        )
      }
  }

  object query extends QueryEncoder
}
