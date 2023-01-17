// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.instances

import cats.data.NonEmptySet
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import lucuma.odb.graphql.client.Itc
import lucuma.odb.json.time

import java.time.Duration


object ItcResultEncoder extends ItcResultEncoder


/**
 * ItcResult Encoder matching the ODB Schema.
 */
trait ItcResultEncoder {

  import time.query.given

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

}