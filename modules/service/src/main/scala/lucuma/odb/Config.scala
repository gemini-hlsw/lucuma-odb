// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import buildinfo.BuildInfo
import cats.Show
import cats.effect.*
import cats.syntax.all.*
import ciris.*
import ciris.refined.*
import com.comcast.ip4s.Port
import eu.timepit.refined.types.string.NonEmptyString
import fs2.aws.s3.models.Models.BucketName
import fs2.aws.s3.models.Models.FileKey
import fs2.io.net.Network
import lucuma.core.data.EmailAddress
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Gid
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
import org.http4s.syntax.all.*
import org.typelevel.log4cats.Logger

import java.net.URI
import java.net.URISyntaxException
import java.security.PublicKey
import java.util.UUID
import scala.concurrent.duration.*

case class Config(
  port:          Port,                      // Our port, nothing fancy.
  itc:           Config.Itc,                // ITC config
  sso:           Config.Sso,                // SSO config
  serviceJwt:    String,                    // Only service users can exchange API keys, so we need a service user JWT.
  honeycomb:     Option[Config.Honeycomb],  // Honeycomb config
  database:      Config.Database,           // Database config
  aws:           Config.Aws,                // AWS config
  email:         Config.Email,              // Mailgun config
  corsOverHttps: Boolean,                   // Whether to require CORS over HTTPS
  domain:        List[String],              // Domains, for CORS headers
  commitHash:    CommitHash,                // From Heroku Dyno Metadata
  goaUsers:      Set[User.Id],              // Gemini Observatory Archive user id(s)
  obscalcPoll:   FiniteDuration,            // Obscalc poll period
  httpClient:    Config.HttpClient          // Configuration for HTTP requests made by the ODB
):

  // People send us their JWTs. We need to be able to extract them from the request, decode them,
  // verify the signature using the SSO server's public key, and then extract the user.
  def jwtReader[F[_]: Concurrent]: SsoJwtReader[F] =
    SsoJwtReader(JwtDecoder.withPublicKey(sso.publicKey))

  // People also send us their API keys. We need to be able to exchange them for [longer-lived] JWTs
  // via an API call to SSO, so we need an HTTP client for that.
  def httpClientResource[F[_]: Async: Network]: Resource[F, Client[F]] =
    EmberClientBuilder.default[F]
      .withTimeout(httpClient.timeout)
      .withIdleConnectionTime(httpClient.effectiveIdleConnectionTime)
      .build

  // ITC client resource
  def itcClient[F[_]: Async: Logger: Network]: Resource[F, ItcClient[F]] =
    httpClientResource[F].evalMap: httpClient =>
      ItcClient.create(itc.root, httpClient)

  // SSO Client resource (has to be a resource because it owns an HTTP client).
  def ssoClient[F[_]: Async: Trace: Network: Logger]: Resource[F, SsoClient[F, User]] =
    httpClientResource[F].evalMap: httpClient =>
      SsoClient.initial(
        serviceJwt = serviceJwt,
        ssoRoot    = sso.root,
        jwtReader  = jwtReader[F],
        httpClient = NatchezMiddleware.client(httpClient), // Note!
      ) .map(_.map(_.user))


object Config:

  case class Itc(
    root:       Uri,             // Root URI for the ITC server we're using.
    pollPeriod: FiniteDuration   // How often to poll for ITC version updates.
  )

  object Itc:
    lazy val fromCiris: ConfigValue[Effect, Itc] = (
      envOrProp("ODB_ITC_ROOT").as[Uri],
      envOrProp("ODB_ITC_POLL_PERIOD").as[FiniteDuration].default(5.minutes)
    ).parMapN(Itc.apply)

  case class Sso(
    root:      Uri,       // Root URI for the SSO server we're using.
    publicKey: PublicKey  // We need to verify user JWTs, which requires the SSO server's public key.
  )

  object Sso:
    lazy val fromCiris: ConfigValue[Effect, Sso] = (
      envOrProp("ODB_SSO_ROOT").as[Uri],
      envOrProp("ODB_SSO_PUBLIC_KEY").as[PublicKey]
    ).parMapN(Sso.apply)

  case class Honeycomb(
    writeKey: String,
    dataset:  String
  )

  object Honeycomb:
    private val inHeroku: ConfigValue[Effect, Boolean] =
      envOrProp("DYNO").option.map(_.isDefined)

    lazy val fromCiris: ConfigValue[Effect, Option[Honeycomb]] =
      inHeroku.flatMap: inHeroku =>
        if (inHeroku)
          (envOrProp("ODB_HONEYCOMB_WRITE_KEY"), envOrProp("ODB_HONEYCOMB_DATASET")).parMapN: (writeKey, dataset) =>
            Honeycomb(writeKey, dataset).some
        else
          (envOrProp("ODB_HONEYCOMB_WRITE_KEY").option, envOrProp("ODB_HONEYCOMB_DATASET").option).parTupled.map:
            case (Some(writeKey), Some(dataset)) if writeKey.trim.nonEmpty && dataset.trim.nonEmpty =>
              Honeycomb(writeKey, dataset).some
            case _ =>
              None

  case class Database(
    maxConnections: Int,
    maxCalibrationConnections: Int,
    maxObscalcConnections: Int,
    host: String,
    port: Int,
    database: String,
    user: String,
    password: String,
  ):
    // We use Flyway (which uses JDBC) to perform schema migrations. Savor the irony.
    def jdbcUrl: String = s"jdbc:postgresql://${host}:${port}/${database}?sslmode=require"

  object Database:

    object Default:
      val MaxConnections = Runtime.getRuntime.availableProcessors * 2 + 1

    // postgres://username:password@host:port/database name
    def fromHerokuUri(
      maxConnections: Int,
      maxCalibrationConnections: Int,
      maxObscalcConnections: Int,
      uri: URI
    ): Option[Database] =
      uri.getUserInfo.split(":") match
        case Array(user, password) =>
          Some(Database(
            maxConnections            = maxConnections,
            maxCalibrationConnections = maxCalibrationConnections,
            maxObscalcConnections     = maxObscalcConnections,
            host     = uri.getHost,
            port     = uri.getPort,
            database = uri.getPath.drop(1),
            user     = user,
            password = password,
          ))
        case _ => None

    private given ConfigDecoder[String, URI] =
      ConfigDecoder[String].mapOption("URI"): s =>
        try Some(new URI(s))
        catch { case _: URISyntaxException => None }

    private given Show[URI] =
      Show.fromToString

    private given ConfigDecoder[(Int, Int, Int, URI), Database] =
      ConfigDecoder[(Int, Int, Int, URI)].mapOption("Database")(Database.fromHerokuUri)

    lazy val fromCiris: ConfigValue[Effect, Database] = (
      envOrProp("ODB_MAX_CONNECTIONS").as[Int].default(Default.MaxConnections),
      envOrProp("CALIBRATIONS_MAX_CONNECTIONS").as[Int].default(Default.MaxConnections),
      envOrProp("OBSCALC_MAX_CONNECTIONS").as[Int].default(Default.MaxConnections),
      envOrProp("DATABASE_URL").as[URI] // passed by Heroku
    ).parTupled.as[Database]

  case class Aws(
    accessKey:       NonEmptyString,
    secretKey:       NonEmptyString,
    basePath:        NonEmptyString,
    bucketName:      BucketName,
    fileUploadMaxMb: Int
  ):
    // Within the ODB we work with the filepath, which is the S3 FileKey
    // minus the basePath. The s3FileService adds the basePath using
    // `fileKey` as needed.
    def filePath(programId: Program.Id, remoteId: UUID, fileName: NonEmptyString): NonEmptyString =
      NonEmptyString.unsafeFrom(s"$programId/$remoteId/$fileName")

    def fileKey(path: NonEmptyString): FileKey =
      FileKey(NonEmptyString.unsafeFrom(s"$basePath/$path"))

  object Aws:
    private given Show[Uri.Path] = Show.fromToString

    private given ConfigDecoder[Uri, Uri.Path] =
      ConfigDecoder[Uri].mapOption("Path")(_.path.some)

    // The basePath must be nonEmpty
    private given ConfigDecoder[Uri.Path, NonEmptyString] =
      ConfigDecoder[Uri.Path].mapOption("NonEmptyPath"): p =>
        val str = p.segments.map(_.encoded).mkString("/")
        NonEmptyString.from(str).toOption

    private given ConfigDecoder[Uri, BucketName] =
      ConfigDecoder[Uri].mapOption("BucketName"): uri =>
        uri
          .host
          .flatMap(_.value.split("\\.").headOption)
          .flatMap: b =>
            NonEmptyString.from(b).toOption.map(BucketName.apply)

    lazy val fromCiris: ConfigValue[Effect, Aws] = (
      envOrProp("CLOUDCUBE_ACCESS_KEY_ID").as[NonEmptyString],
      envOrProp("CLOUDCUBE_SECRET_ACCESS_KEY").as[NonEmptyString].redacted,
      envOrProp("CLOUDCUBE_URL").as[Uri].as[Uri.Path].as[NonEmptyString],
      envOrProp("CLOUDCUBE_URL").as[Uri].as[BucketName],
      envOrProp("FILE_UPLOAD_MAX_MB").as[Int]
    ).parMapN(Aws.apply)

  case class Email(
    apiKey:            NonEmptyString,
    domain:            NonEmptyString,
    webhookSigningKey: NonEmptyString,
    invitationFrom:    EmailAddress,
    exploreUrl:        Uri
  ):
    // add to environment?
    lazy val baseUri = uri"https://api.mailgun.net/v3"
    lazy val sendMessageUri = baseUri / domain.value / "messages"
    lazy val eventsUri = baseUri / domain.value / "events"

  object Email:
    lazy val fromCiris: ConfigValue[Effect, Email] = (
      envOrProp("MAILGUN_API_KEY").as[NonEmptyString],
      envOrProp("MAILGUN_DOMAIN").as[NonEmptyString],
      envOrProp("MAILGUN_WEBHOOK_SIGNING_KEY").as[NonEmptyString],
      envOrProp("INVITATION_SENDER_EMAIL").as[EmailAddress],
      envOrProp("EXPLORE_URL").as[Uri]
    ).parMapN(Email.apply)

    private given ConfigDecoder[String, EmailAddress] =
      ConfigDecoder[String].mapOption("Email Address"): s =>
        EmailAddress.from(s).toOption

  case class HttpClient(
    timeout: Duration,
    idleConnectionTime: Duration
  ):
    val effectiveIdleConnectionTime: Duration =
      if idleConnectionTime > timeout
        then idleConnectionTime
        else timeout.plus(1.second)

  object HttpClient:
    lazy val fromCiris: ConfigValue[Effect, HttpClient] = (
      envOrProp("HTTP_CLIENT_TIMEOUT").as[Duration].default(45.seconds), // 45s is Ember's default
      envOrProp("HTTP_CLIENT_IDLE_CONNECTION_TIME").as[Duration].default(60.seconds) // 60s is Ember's default
    ).parMapN(HttpClient.apply)

  private given ConfigDecoder[String, PublicKey] =
    ConfigDecoder[String].mapOption("Public Key"): s =>
      GpgPublicKeyReader.publicKey(s).toOption

  private given ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI"): s =>
      Uri.fromString(s).toOption

  private given ConfigDecoder[Int, Port] =
    ConfigDecoder[Int].mapOption("Port")(Port.fromInt)

  private given ConfigDecoder[String, CommitHash] =
    ConfigDecoder[String].mapOption("CommitHash")(CommitHash.FromString.getOption)

  private given [A](using Gid[A]): ConfigDecoder[String, A] =
    ConfigDecoder[String].mapOption("Gid[A]")(Gid[A].fromString.getOption)

  private given [A](using ConfigDecoder[String, A]): ConfigDecoder[String, List[A]] =
    ConfigDecoder[String].map(_.split(",").map(_.trim).toList).mapEither: (key, ss) =>
      ss.traverse(ConfigDecoder[String, A].decode(key, _))

  private def envOrProp(name: String): ConfigValue[Effect, String] =
    env(name) or prop(name)

  private def optValue[A](value: => Option[A]): ConfigValue[Effect, A] =
    value.fold(ConfigValue.failed(ConfigError("Missing value")))(default)

  lazy val fromCiris: ConfigValue[Effect, Config] = (
    envOrProp("PORT").as[Int].as[Port], // passed by Heroku
    Itc.fromCiris,
    Sso.fromCiris,
    envOrProp("ODB_SERVICE_JWT"),
    Honeycomb.fromCiris,
    Database.fromCiris,
    Aws.fromCiris,
    Email.fromCiris,
    envOrProp("CORS_OVER_HTTPS").as[Boolean].default(true), // By default require https
    envOrProp("ODB_DOMAIN").as[List[String]],
    optValue(BuildInfo.gitHeadCommit).as[CommitHash].default(CommitHash.Zero),
    envOrProp("GOA_USER_IDS").as[List[User.Id]].map(_.toSet).default(Set.empty),
    envOrProp("OBSCALC_POLL_SECONDS").as[FiniteDuration].default(10.seconds),
    HttpClient.fromCiris
  ).parMapN(Config.apply)
