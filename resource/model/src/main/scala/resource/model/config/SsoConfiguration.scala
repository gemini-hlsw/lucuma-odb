// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model.config

import cats.Eq
import cats.effect.Concurrent
import cats.syntax.all.*
import ciris.*
import ciris.http4s.*
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.client.util.GpgPublicKeyReader
import lucuma.sso.client.util.JwtDecoder
import org.http4s.Uri

import java.security.PublicKey

final case class SsoConfiguration(
  root:       Uri,
  publicKey:  PublicKey,
  serviceJwt: String
):
  def jwtReader[F[_]: Concurrent]: SsoJwtReader[F] =
    SsoJwtReader(JwtDecoder.withPublicKey(publicKey))

object SsoConfiguration:

  given Eq[SsoConfiguration] =
    Eq.by(c => (c.root.renderString, c.serviceJwt))

  private given ConfigDecoder[String, PublicKey] =
    ConfigDecoder[String].mapEither: (_, s) =>
      GpgPublicKeyReader.publicKey(s).leftMap(e => ConfigError(e.getMessage()))

  lazy val fromCiris: ConfigValue[Effect, SsoConfiguration] = (
    envOrProp("RESOURCE_SSO_ROOT").as[Uri],
    envOrProp("RESOURCE_SSO_PUBLIC_KEY").as[PublicKey],
    envOrProp("RESOURCE_SSO_SERVICE_JWT").redacted
  ).parMapN(SsoConfiguration.apply)
