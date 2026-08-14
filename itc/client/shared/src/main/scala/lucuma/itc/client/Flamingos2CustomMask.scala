// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.eq.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.itc.client.json.syntax.*

final case class Flamingos2CustomMask(slitWidth: Flamingos2CustomSlitWidth)

object Flamingos2CustomMask:

  given Encoder[Flamingos2CustomMask] with
    def apply(a: Flamingos2CustomMask): Json =
      Json.obj(
        "slitWidth" -> a.slitWidth.asScreamingJson
      )

  given Decoder[Flamingos2CustomMask] with
    def apply(c: HCursor): Decoder.Result[Flamingos2CustomMask] =
      c.downField("slitWidth").as[Flamingos2CustomSlitWidth].map(Flamingos2CustomMask(_))

  given Eq[Flamingos2CustomMask] with
    def eqv(x: Flamingos2CustomMask, y: Flamingos2CustomMask): Boolean =
      x.slitWidth === y.slitWidth
