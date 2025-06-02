// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.data.NonEmptyChain
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.ObservationValidation

trait ObservationValidationCodec:

  given Decoder[ObservationValidation] = hc =>
    for
      c   <- hc.downField("code").as[ObservationValidationCode]
      ms  <- hc.downField("messages").as[List[String]]
      nec <- NonEmptyChain.fromSeq(ms).toRight(DecodingFailure("Missing messages", hc.history))
    yield ObservationValidation(c, nec)

  given Encoder[ObservationValidation] = c =>
    Json.obj(
      "code"     -> c.code.asJson,
      "messages" -> c.messages.toNonEmptyList.toList.asJson
    )

object observationvalidation extends ObservationValidationCodec