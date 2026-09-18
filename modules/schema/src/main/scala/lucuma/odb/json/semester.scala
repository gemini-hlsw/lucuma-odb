// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Semester

trait SemesterCodec {

  given Codec[Semester] with {
    def apply(s: Semester): Json =
      s.format.asJson

    def apply(c: HCursor): Decoder.Result[Semester] =
      Decoder[String].apply(c).flatMap { s =>
        Semester.fromString
          .getOption(s)
          .toRight(DecodingFailure(s"Invalid semester value: $s", c.history))
      }
  }
}

object semester extends SemesterCodec
