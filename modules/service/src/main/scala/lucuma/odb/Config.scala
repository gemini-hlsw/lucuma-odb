// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.Show
import cats.effect._
import cats.syntax.all._
import ciris._
import com.comcast.ip4s.Port
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.sequence.util.CommitHash
import lucuma.sso.client.SsoClient
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.client.util.GpgPublicKeyReader
import lucuma.sso.client.util.JwtDecoder
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.Logger

import java.net.URI
import java.net.URISyntaxException
import java.security.PublicKey

case class Config(
  port:       Port,             // Our port, nothing fancy.
  itcRoot:    Uri,              // ITC service URI
  sso:        Config.Sso,       // SSO config
  serviceJwt: String,           // Only service users can exchange API keys, so we need a service user JWT.
  honeycomb:  Config.Honeycomb, // Honeycomb config
  database:   Config.Database,  // Database config
  domain:     String,           // Domain, for CORS headers
  commitHash: CommitHash        // From Heroku Dyno Metadata
) {

  // People send us their JWTs. We need to be able to extract them from the request, decode them,
  // verify the signature using the SSO server's public key, and then extract the user.
  def jwtReader[F[_]: Concurrent]: SsoJwtReader[F] =
    SsoJwtReader(JwtDecoder.withPublicKey(sso.publicKey))

  // People also send us their API keys. We need to be able to exchange them for [longer-lived] JWTs
  // via an API call to SSO, so we need an HTTP client for that.
  def httpClientResource[F[_]: Async]: Resource[F, Client[F]] =
    EmberClientBuilder.default[F].build

  // ITC client resource
  def itcClient[F[_]: Async: Logger]: Resource[F, ItcClient[F]] =
    httpClientResource[F].evalMap { httpClient =>
      ItcClient.create(itcRoot, httpClient)
    }

  // SSO Client resource (has to be a resource because it owns an HTTP client).
  def ssoClient[F[_]: Async: Trace]: Resource[F, SsoClient[F, User]] =
    httpClientResource[F].evalMap { httpClient =>
      SsoClient.initial(
        serviceJwt = serviceJwt,
        ssoRoot    = sso.root,
        jwtReader  = jwtReader[F],
        httpClient = NatchezMiddleware.client(httpClient), // Note!
      ) .map(_.map(_.user))
    }

}


object Config {

  case class Sso(
    root:      Uri,       // Root URI for the SSO server we're using.
    publicKey: PublicKey  // We need to verify user JWTs, which requires the SSO server's public key.
  )

  object Sso {

    val fromCiris: ConfigValue[Effect, Sso] = (
      envOrProp("ODB_SSO_ROOT").as[Uri],
      envOrProp("ODB_SSO_PUBLIC_KEY").as[PublicKey]
    ).parMapN(Sso.apply)

  }

  case class Honeycomb(
    writeKey: String,
    dataset:  String
  )

  object Honeycomb {

    val fromCiris: ConfigValue[Effect, Honeycomb] = (
      envOrProp("ODB_HONEYCOMB_WRITE_KEY"),
      envOrProp("ODB_HONEYCOMB_DATASET")
    ).parMapN(Honeycomb.apply)

  }

  case class Database(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: String,
  ) {
    // We use Flyway (which uses JDBC) to perform schema migrations. Savor the irony.
    def jdbcUrl: String = s"jdbc:postgresql://${host}:${port}/${database}?sslmode=require"
  }

  object Database {

    // postgres://username:password@host:port/database name
    def fromHerokuUri(uri: URI): Option[Database] =
      uri.getUserInfo.split(":") match {
        case Array(user, password) =>
          Some(Database(
            host     = uri.getHost,
            port     = uri.getPort,
            database = uri.getPath.drop(1),
            user     = user,
            password = password,
          ))
        case _ => None
      }

    private implicit val uri: ConfigDecoder[String, URI] =
      ConfigDecoder[String].mapOption("URI") { s =>
        try Some(new URI(s))
        catch { case _: URISyntaxException => None }
      }

    private implicit val ShowURI: Show[URI] =
      Show.fromToString

    private implicit val ConfigDecoderDatabaseConfig: ConfigDecoder[URI, Database] =
      ConfigDecoder[URI].mapOption("Database")(Database.fromHerokuUri)

    lazy val fromCiris: ConfigValue[Effect, Database] =
      envOrProp("DATABASE_URL").as[URI].as[Database] // passed by Heroku

  }

  private implicit val publicKey: ConfigDecoder[String, PublicKey] =
    ConfigDecoder[String].mapOption("Public Key") { s =>
      GpgPublicKeyReader.publicKey(s).toOption
    }

  private implicit val uri: ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI") { s =>
      Uri.fromString(s).toOption
    }

  private implicit val port: ConfigDecoder[Int, Port] =
    ConfigDecoder[Int].mapOption("Port")(Port.fromInt)

  private implicit val commitHash: ConfigDecoder[String, CommitHash] =
    ConfigDecoder[String].mapOption("CommitHash")(CommitHash.FromString.getOption)

  private def envOrProp(name: String): ConfigValue[Effect, String] =
    env(name) or prop(name)

  val fromCiris: ConfigValue[Effect, Config] = (
    envOrProp("PORT").as[Int].as[Port], // passed by Heroku
    envOrProp("ODB_ITC_ROOT").as[Uri],
    Sso.fromCiris,
    envOrProp("ODB_SERVICE_JWT"),
    Honeycomb.fromCiris,
    Database.fromCiris,
    envOrProp("ODB_DOMAIN"),
    envOrProp("HEROKU_SLUG_COMMIT").as[CommitHash].default(CommitHash.Zero)
  ).parMapN(Config.apply)

}

