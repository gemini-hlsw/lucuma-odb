// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.instances

import cats.data.NonEmptySet
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import lucuma.odb.graphql.client.Itc

import java.time.Duration


object ItcResultEncoder extends ItcResultEncoder


/**
 * ItcResult Encoder matching the ODB Schema.
 */
trait ItcResultEncoder {
  private val MissingParams: String = "MISSING_PARAMS"
  private val ServiceError:  String = "SERVICE_ERROR"
  private val Success:       String = "SUCCESS"

  private def status(r: Itc.Result): String =
    r.fold(_ => MissingParams, _ => ServiceError, _ => Success)

  given Encoder[Itc.Param] =
    Encoder.instance(_.stringValue.asJson)

  given [A: Encoder]: Encoder[NonEmptySet[A]] =
    Encoder.instance(_.toNonEmptyList.toList.asJson)

  given Encoder[Itc.Result] with {
    def apply(r: Itc.Result): Json =
      Json.fromFields(
        ("status" -> status(r).asJson) :: r.fold(
          m => List(
            "targetId" -> m.targetId.asJson,
            "params"   -> m.params.asJson
          ),
          e => List(
            "targetId" -> e.targetId.asJson,
            "message"  -> e.message.asJson
          ),
          s => List(
            "targetId"      -> s.targetId.asJson,
            "exposureTime"  -> s.exposureTime.value.asJson,
            "exposures"     -> s.exposures.value.asJson,
            "signalToNoise" -> s.signalToNoise.value.asJson
          )
        )
      )
  }

  given Encoder[Itc.ResultSet] with {
    def apply(r: Itc.ResultSet): Json =
      Json.obj(
        "programId"     -> r.programId.asJson,
        "observationId" -> r.observationId.asJson,
        "result"        -> r.value.focus.asJson,
        "all"           -> r.value.toList.asJson
      )
  }

  given Encoder[Duration] with {
    def apply(d: Duration): Json = {

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
  }

}