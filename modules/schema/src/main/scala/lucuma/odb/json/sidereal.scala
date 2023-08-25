// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Epoch
import lucuma.core.model.Target.Sidereal

object sidereal {

  import cataloginfo.query.given
  import declination.query.given
  import parallax.query.given
  import propermotion.query.given
  import radialvelocity.query.given
  import rightascension.query.given

  trait QueryEncoder {

    given Encoder[Sidereal] =
      Encoder.instance { s =>
        Json.obj(
          "ra"             -> s.tracking.baseCoordinates.ra.asJson,
          "dec"            -> s.tracking.baseCoordinates.dec.asJson,
          "epoch"          -> Epoch.fromString.reverseGet(s.tracking.epoch).asJson,
          "properMotion"   -> s.tracking.properMotion.asJson,
          "radialVelocity" -> s.tracking.radialVelocity.asJson,
          "parallax"       -> s.tracking.parallax.asJson,
          "catalogInfo"    -> s.catalogInfo.asJson
        )
      }
  }

  object query extends QueryEncoder
}
