// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.instances

import lucuma.itc.client.ItcResult

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._

import java.time.Duration


object ItcResultEncoder extends ItcResultEncoder


/**
 * ItcResult Encoder matching the ODB Schema.
 */
trait ItcResultEncoder {

  given Encoder[ItcResult.Success] with {

    def duration(d: Duration): Json = {

      import java.math.RoundingMode.DOWN
      import java.time.temporal.ChronoUnit.MICROS

      val d2 = d.truncatedTo(MICROS)

      val micro = BigDecimal(
        new java.math.BigDecimal(d2.getSeconds)
          .movePointRight(9)
          .add(new java.math.BigDecimal(d2.getNano))
          .movePointLeft(3)
          .setScale(0, DOWN)
      )

      Json.obj(
        "microseconds" -> micro.longValue.asJson,
        "milliseconds" -> (micro / 1_000L).asJson,
        "seconds"      -> (micro / 1_000_000L).asJson,
        "minutes"      -> (micro / 60_000_000L).asJson,
        "hours"        -> (micro / 3_600_000_000L).asJson,
        "iso"          -> d2.toString.asJson
      )
    }


    def apply(r: ItcResult.Success): Json =
      Json.obj(
        "exposureTime"  -> duration(r.exposureTime.value),
        "exposures"     -> r.exposures.value.asJson,
        "signalToNoise" -> r.signalToNoise.value.asJson
      )
  }
}