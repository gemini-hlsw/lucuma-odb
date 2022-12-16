// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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

  private def targetResultStatus(r: Itc.TargetResult): String =
    r.result.fold(_ => MissingParams, _ => ServiceError, _ => Success)

  given Encoder[Itc.Param] =
    Encoder.instance(_.stringValue.asJson)

  given [A: Encoder]: Encoder[NonEmptySet[A]] =
    Encoder.instance(_.toNonEmptyList.toList.asJson)

  given Encoder[Itc.TargetResult] with {
    def apply(r: Itc.TargetResult): Json =
      Json.obj(
        "targetId"      -> r.targetId.asJson,
        "status"        -> targetResultStatus(r).asJson,
        "missingParams" -> r.result.missing.map(_.params).asJson,
        "serviceError"  -> r.result.serviceError.map(_.message).asJson,
        "success"       -> r.result.success.asJson
      )
  }

  given Encoder[Itc.ObservationResult] with {
    def apply(r: Itc.ObservationResult): Json =
      Json.obj(
        "programId"     -> r.programId.asJson,
        "observationId" -> r.observationId.asJson,
        "status"        -> r.value.fold(_ => MissingParams, z => targetResultStatus(z.focus)).asJson,
        "missingParams" -> r.value.left.toOption.map(_.params).asJson,
        "selected"      -> r.value.toOption.map(_.focus).asJson,
        "all"           -> r.value.toOption.toList.flatMap(_.toList).asJson
      )
  }

  given Encoder[Itc.Result.Success] with {

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


    def apply(r: Itc.Result.Success): Json =
      Json.obj(
        "exposureTime"  -> duration(r.exposureTime.value),
        "exposures"     -> r.exposures.value.asJson,
        "signalToNoise" -> r.signalToNoise.value.asJson
      )
  }
}