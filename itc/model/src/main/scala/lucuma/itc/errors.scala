// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import lucuma.core.util.Enumerated

enum ErrorCode(val tag: String) derives Enumerated:
  case SourceTooBright extends ErrorCode("source_too_bright")
  case General         extends ErrorCode("general")

enum Error(val code: ErrorCode, val message: String):
  case SourceTooBright(wellHalfFilledSeconds: BigDecimal)
      extends Error(
        ErrorCode.SourceTooBright,
        f"Source too bright, well half filled in $wellHalfFilledSeconds%.2f seconds"
      )
  case General(override val message: String) extends Error(ErrorCode.General, message)

object Error:
  given Eq[Error] = Eq.instance:
    case (SourceTooBright(hw1), SourceTooBright(hw2)) => hw1 === hw2
    case (General(m1), General(m2))                   => m1 === m2
    case _                                            => false

  given Encoder[Error] = e =>
    Json
      .obj(
        "errorCode" -> e.code.asJson,
        "message"   -> e.message.asJson
      )
      .deepMerge(e match
        case SourceTooBright(hw) => Json.obj("wellHalfFilledSeconds" -> hw.asJson)
        case _                   => Json.obj("wellHalfFilledSeconds" -> Json.Null))

  given Decoder[Error] = c =>
    c.downField("errorCode")
      .as[ErrorCode]
      .flatMap:
        case ErrorCode.SourceTooBright =>
          c.downField("wellHalfFilledSeconds").as[BigDecimal].map(SourceTooBright(_))
        case _                         =>
          c.downField("message").as[String].map(General(_))

case class ErrorExtension(targetIndex: Int, error: Error) derives Encoder.AsObject, Decoder
