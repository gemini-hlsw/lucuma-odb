// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package orcid

package lucuma.sso.service.orcid

import cats.effect.Concurrent
import io.circe.Decoder
import org.http4s.EntityDecoder
import org.http4s.circe.*

final case class OrcidException(error: String, description: String)
  extends Exception(s"$error: $description")

object OrcidException {

  implicit val DecoderOrcidException: Decoder[OrcidException] = c =>
    for {
      error <- c.downField("error").as[String]
      desc  <- c.downField("error_description").as[String]
    } yield OrcidException(error, desc)

  implicit def entityDecoderOrcidPerson[F[_]: Concurrent]: EntityDecoder[F, OrcidException] =
    jsonOf[F, OrcidException]

}