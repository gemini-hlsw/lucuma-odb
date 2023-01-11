// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.Json
import io.circe.refined._
import io.circe.syntax._
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle

trait AngleCodec {

  given Codec[Angle] with {

    def apply(a: Angle): Json = {

      val ha = Angle.hourAngle.get(a)

      def convertAngle(div: Int): Json =
        (BigDecimal(a.toMicroarcseconds) / div).asJson

      def convertHourAngle(div: Int): Json =
        (BigDecimal(ha.toMicroseconds) / div).asJson

      Json.obj(
        "microarcseconds" -> a.toMicroarcseconds.asJson,
        "microseconds"    -> ha.toMicroseconds.asJson,
        "milliarcseconds" -> convertAngle(1_000),
        "milliseconds"    -> convertHourAngle(1_000),
        "arcseconds"      -> convertAngle(1_000_000),
        "seconds"         -> convertHourAngle(1_000_000),
        "arcminutes"      -> convertAngle(60 * 1_000_000),
        "minutes"         -> convertHourAngle(60 * 1_000_000),
        "degrees"         -> convertAngle(3_600 * 1_000_000),
        "hours"           -> convertHourAngle(3_600 * 1_000_000),
        "hms"             -> HourAngle.HMS(ha).format.asJson,
        "dms"             -> Angle.dms.get(a).format.asJson
      )
    }

    def apply(c: HCursor): Decoder.Result[Angle] =
      c.downField("microarcseconds").as[Long].map(Angle.fromMicroarcseconds)

  }

}

object angle extends AngleCodec

