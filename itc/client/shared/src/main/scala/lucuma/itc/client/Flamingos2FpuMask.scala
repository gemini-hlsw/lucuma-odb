// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Fpu
import lucuma.itc.client.json.syntax.*

final case class Flamingos2FpuMask(fpu: Either[Flamingos2CustomMask, Flamingos2Fpu]) {
  def customMask: Option[Flamingos2CustomMask] =
    fpu.left.toOption

  def builtin: Option[Flamingos2Fpu] =
    fpu.toOption
}

object Flamingos2FpuMask:

  def customMask(m: Flamingos2CustomMask): Flamingos2FpuMask =
    Flamingos2FpuMask(m.asLeft)

  def builtin(b: Flamingos2Fpu): Flamingos2FpuMask =
    Flamingos2FpuMask(b.asRight)

  given Encoder[Flamingos2FpuMask] with
    def apply(a: Flamingos2FpuMask): Json =
      Json.obj(
        a.fpu.fold(
          m => "customMask" -> m.asJson,
          b => "builtin"    -> b.asScreamingJson
        )
      )

  given Decoder[Flamingos2FpuMask] = (c: HCursor) =>
    for
      m <- c.downField("customMask").as[Option[Flamingos2CustomMask]]
      b <- c.downField("builtin").as[Option[Flamingos2Fpu]]
      u <- (m, b) match
             case (Some(m), None) => m.asLeft[Flamingos2Fpu].asRight[DecodingFailure]
             case (None, Some(b)) => b.asRight[Flamingos2CustomMask].asRight[DecodingFailure]
             case _               =>
               DecodingFailure("Expected exactly one of `customMask` or `builtin`",
                               c.history
               ).asLeft
    yield Flamingos2FpuMask(u)

  given Eq[Flamingos2FpuMask] = Eq.by(_.fpu)
