// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client.codec

import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.util.Gid

trait GidCodec {

  implicit def gidEncoder[A](implicit ev: Gid[A]): Encoder[A] =
    Encoder[String].contramap(ev.fromString.reverseGet)

  implicit def gidDecoder[A](implicit ev: Gid[A]): Decoder[A] =
    Decoder[String].emap(s => ev.fromString.getOption(s).toRight(s"Invalid GID: $s"))

}

object gid extends GidCodec
