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
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.util.Enumerated
import lucuma.itc.client.json.syntax.*

object GmosFpu {

  final case class North(fpu: Either[GmosCustomMask, GmosNorthFpu]) {
    def customMask: Option[GmosCustomMask] =
      fpu.left.toOption

    def builtin: Option[GmosNorthFpu] =
      fpu.toOption
  }

  object North {

    def customMask(m: GmosCustomMask): North =
      North(m.asLeft)

    def builtin(b: GmosNorthFpu): North =
      North(b.asRight)

    given Encoder[North] with
      def apply(a: North): Json =
        json(a.fpu)

    given Decoder[North] =
      decoder[North, GmosNorthFpu](North(_))

    given Eq[North] with
      def eqv(x: North, y: North): Boolean =
        x.fpu === y.fpu

  }

  final case class South(fpu: Either[GmosCustomMask, GmosSouthFpu]) {
    def customMask: Option[GmosCustomMask] =
      fpu.left.toOption

    def builtin: Option[GmosSouthFpu] =
      fpu.toOption
  }

  object South {

    def customMask(m: GmosCustomMask): South =
      South(m.asLeft)

    def builtin(b: GmosSouthFpu): South =
      South(b.asRight)

    given Encoder[South] with
      def apply(a: South): Json =
        json(a.fpu)

    given Decoder[South] =
      decoder[South, GmosSouthFpu](South(_))

    given Eq[South] with
      def eqv(x: South, y: South): Boolean =
        x.fpu === y.fpu

  }

  private def json[U: Enumerated](e: Either[GmosCustomMask, U]): Json =
    Json.obj(
      e.fold(
        m => "customMask" -> m.asJson,
        b => "builtin" -> b.asScreamingJson
      )
    )

  private def decoder[A, U: Enumerated](f: (Either[GmosCustomMask, U] => A)): Decoder[A] =
    new Decoder[A] {
      def apply(c: HCursor): Decoder.Result[A] =
        for {
          m <- c.downField("customMask").as[Option[GmosCustomMask]]
          b <- c.downField("builtin").as[Option[U]]
          u <- (m, b) match {
                 case (Some(m), None) => m.asLeft[U].asRight[DecodingFailure]
                 case (None, Some(b)) => b.asRight[GmosCustomMask].asRight[DecodingFailure]
                 case _               =>
                   DecodingFailure("Expected exactly one of `customMask` or `builtin`",
                                   c.history
                   ).asLeft
               }
        } yield f(u)
    }

}
