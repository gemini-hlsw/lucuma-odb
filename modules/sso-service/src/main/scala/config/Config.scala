// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.config

import cats.effect.*
import cats.syntax.all.*
import ciris.*
import com.comcast.ip4s.Port
import lucuma.common.config.Environment
import lucuma.common.config.Environment.Local
import lucuma.otel.OtelConfig
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.client.util.JwtDecoder
import lucuma.sso.service.SsoJwtWriter
import lucuma.sso.service.util.JwtEncoder
import org.http4s.Uri
import org.http4s.Uri.Authority
import org.http4s.Uri.RegName

import java.security.KeyPairGenerator
import java.security.PrivateKey
import java.security.PublicKey
import java.security.SecureRandom
import scala.concurrent.duration.*

final case class Config(
  environment:  Environment,
  database:     DatabaseConfig,
  orcid:        OrcidConfig,
  publicKey:    PublicKey,
  privateKey:   PrivateKey,
  httpPort:     Port,
  cookieDomain: String,
  scheme:       Uri.Scheme,
  hostname:     String,
  heroku:       Option[HerokuConfig],
  otel:         Option[OtelConfig],
  odbHostName:  String,
) {

  def versionText: String =
    heroku.fold(s"Work In Progress")(_.versionText)

  val authority: Uri.Authority =
    Uri.Authority(
      host = RegName(hostname),
      port = Some(httpPort.value)
    )

  // TODO: parameterize
  val JwtLifetime    = 10.minutes

  def ssoJwtReader[F[_]: Concurrent] =
    SsoJwtReader(JwtDecoder.withPublicKey[F](publicKey))

  def ssoJwtWriter[F[_]: Sync] =
    SsoJwtWriter(JwtEncoder.withPrivateKey[F](privateKey), JwtLifetime)

  def publicUri: Uri =
    Uri(
      scheme = Some(scheme),
      authority = Some(
        Authority(
          host = authority.host,
          port = if (environment.isLocal) Some(httpPort.value) else None
        )
      )
    )

  val odbRootUri = Uri.unsafeFromString(s"https://$odbHostName/")

}

object Config {

  def local(orcid: OrcidConfig, otel: Option[OtelConfig]): Config = {

    // Generate a random key pair. This basically means nobody is going to be able to validate keys
    // issued here because they have no way to get the public key. It may end up being better to use
    // a constant well-known key but of course if that makes it into production all is lost. So we
    // will err on the side of safety for the moment.
    val keyGen  = KeyPairGenerator.getInstance("RSA", "SunRsaSign")
    val random  = SecureRandom.getInstance("SHA1PRNG", "SUN")
    val keyPair = { keyGen.initialize(1024, random); keyGen.generateKeyPair }

    // And our canned config for local development.
    Config(
      Environment.Local,
      DatabaseConfig.Local,
      orcid,
      keyPair.getPublic,
      keyPair.getPrivate,
      Port.fromInt(8080).getOrElse(sys.error("unpossible: invalid port")),
      "local.gemini.edu",
      Uri.Scheme.http,
      "local.gemini.edu",
      None,
      otel,
      "local.gemini.edu"
    )

  }

  def config: ConfigValue[Effect, Config] =
    val environment = Environment.fromCiris("LUCUMA_SSO_ENVIRONMENT")
    (
      environment,
      OrcidConfig.config,
      OtelConfig.fromEnv("LUCUMA_SSO_OTEL", environment.map(_.name))
    ).parFlatMapN {
      case (Local, orcid, otel) => default(local(orcid(Local), otel))
      case (env, orcid, otel)   => deployed(env, orcid(env), otel)
    }

  private def deployed(
    env:   Environment,
    orcid: OrcidConfig,
    otel:  Option[OtelConfig]
  ): ConfigValue[Effect, Config] = (
    envOrProp("PORT").as[Int].as[Port],
    DatabaseConfig.config,
    envOrProp("GPG_SSO_PUBLIC_KEY").as[PublicKey],
    envOrProp("GPG_SSO_PRIVATE_KEY").redacted,
    envOrProp("GPG_SSO_PASSPHRASE").redacted,
    envOrProp("LUCUMA_SSO_COOKIE_DOMAIN"),
    envOrProp("LUCUMA_SSO_HOSTNAME"),
    HerokuConfig.config.option,
    envOrProp("LUCUMA_ODB_HOSTNAME"),
  ).parFlatMapN { (port, dbc, pkey, text, pass, domain, host, heroku, odb) =>
    for {
      skey <- default(text).as[PrivateKey](using privateKey(pass))
    } yield Config(env, dbc, orcid, pkey, skey, port, domain, Uri.Scheme.https, host, heroku, otel, odb)
  }

}
