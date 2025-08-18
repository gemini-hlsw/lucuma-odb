// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.example

import cats.effect.*
import cats.syntax.all.*
import ciris.*
import com.comcast.ip4s.Port
import fs2.io.net.Network
import lucuma.sso.client.SsoClient
import lucuma.sso.client.SsoClient.UserInfo
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.client.util.GpgPublicKeyReader
import lucuma.sso.client.util.JwtDecoder
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.Logger

import java.security.PublicKey

case class Config(
  port:         Port,      // Our port, nothing fancy.
  ssoRoot:      Uri,       // Root URI for the SSO server we're using.
  ssoPublicKey: PublicKey, // We need to verify user JWTs, which requires the SSO server's public key.
  serviceJwt:   String,    // Only service users can exchange API keys, so we need a service user JWT.
  hcWriteKey:   String,    // Honeycomb API key
  hcDataset:    String,    // Honeycomb dataset
) {

  // People send us their JWTs. We need to be able to extract them from the request, decode them,
  // verify the signature using the SSO server's public key, and then extract the user.
  def jwtReader[F[_]: Concurrent]: SsoJwtReader[F] =
    SsoJwtReader(JwtDecoder.withPublicKey(ssoPublicKey))

  // People also send us their API keys. We need to be able to exchange them for [longer-lived] JWTs
  // via an API call to SSO, so we need an HTTP client for that.
  def httpClientResource[F[_]: Async: Network]: Resource[F, Client[F]] =
    EmberClientBuilder.default[F].build

  // SSO Client resource (has to be a resource because it owns an HTTP client).
  def ssoClient[F[_]: Async: Trace: Network: Logger]: Resource[F, SsoClient[F, UserInfo]] =
    httpClientResource[F].evalMap { httpClient =>
      SsoClient.initial(
        serviceJwt = serviceJwt,
        ssoRoot    = ssoRoot,
        jwtReader  = jwtReader[F],
        httpClient = NatchezMiddleware.client(httpClient), // Note!
      )
    }

}


object Config {

  implicit val publicKey: ConfigDecoder[String, PublicKey] =
    ConfigDecoder[String].mapOption("Public Key") { s =>
      GpgPublicKeyReader.publicKey(s).toOption
    }

  implicit val uri: ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI") { s =>
      Uri.fromString(s).toOption
    }

  implicit val port: ConfigDecoder[Int, Port] =
    ConfigDecoder[Int].mapOption("Port")(Port.fromInt)

  def envOrProp(name: String): ConfigValue[Effect, String] =
    env(name) or prop(name)

  val fromCiris = (
    envOrProp("EXAMPLE_PORT").as[Int].as[Port],
    envOrProp("EXAMPLE_SSO_ROOT").as[Uri],
    envOrProp("EXAMPLE_SSO_PUBLIC_KEY").as[PublicKey],
    envOrProp("EXAMPLE_SERVICE_JWT"),
    envOrProp("EXAMPLE_HONEYCOMB_WRITE_KEY"),
    envOrProp("EXAMPLE_HONEYCOMB_DATASET"),
  ).parMapN(Config.apply)

}

