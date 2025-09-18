// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.eq.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.itc.client.json.syntax.*

final case class GmosCustomMask(
  slitWidth: GmosCustomSlitWidth,
  fileName:  String
)

object GmosCustomMask {

  given Encoder[GmosCustomMask] with
    def apply(a: GmosCustomMask): Json =
      Json.obj(
        "slitWidth" -> a.slitWidth.asScreamingJson,
        "filename"  -> a.fileName.asJson // NOTE: all lower case tag "filename"
      )

  given Decoder[GmosCustomMask] with
    def apply(c: HCursor): Decoder.Result[GmosCustomMask] =
      for {
        s <- c.downField("slitWidth").as[GmosCustomSlitWidth]
        f <- c.downField("filename").as[String]
      } yield GmosCustomMask(s, f)

  given Eq[GmosCustomMask] with
    def eqv(x: GmosCustomMask, y: GmosCustomMask): Boolean =
      (x.slitWidth === y.slitWidth) &&
        (x.fileName === y.fileName)
}
