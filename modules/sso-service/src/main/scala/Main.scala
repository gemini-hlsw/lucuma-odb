// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.*
import cats.data.Kleisli
import cats.data.Validated
import cats.effect.*
import cats.effect.std.Console
import cats.implicits.*
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import fs2.io.net.Network
import grackle.skunk.SkunkMonitor
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.util.Gid
import lucuma.sso.service.config.*
import lucuma.sso.service.database.Database
import lucuma.sso.service.graphql.GraphQLRoutes
import lucuma.sso.service.graphql.SsoMapping
import lucuma.sso.service.orcid.OrcidService
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import natchez.http4s.implicits.*
import natchez.log.Log
import org.flywaydb.core.Flyway
import org.flywaydb.core.api.output.MigrateResult
import org.http4s.*
import org.http4s.Uri.Scheme
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits.*
import org.http4s.server.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import skunk.{Command as _, *}

import scala.concurrent.duration.*
import scala.io.AnsiColor

object MainArgs:
  opaque type ResetDatabase = Boolean

  object ResetDatabase:
    val opt: Opts[ResetDatabase] =
      Opts.flag("reset", help = "Drop and recreate the database before starting.").orFalse

    extension (rd: ResetDatabase)
      def toBoolean: Boolean   = rd
      def isRequested: Boolean = toBoolean

  opaque type SkipMigration = Boolean

  object SkipMigration:
    val opt: Opts[SkipMigration] =
      Opts.flag("skip-migration", help = "Skip database migration on startup.").orFalse

    extension (sm: SkipMigration)
      def toBoolean: Boolean   = sm
      def isRequested: Boolean = toBoolean

object Main extends CommandIOApp(
  name    = "lucuma-sso",
  header  =
    s"""|╦  ╦ ╦╔═╗╦ ╦╔╦╗╔═╗   ╔═╗╔═╗╔═╗
        |║  ║ ║║  ║ ║║║║╠═╣───╚═╗╚═╗║ ║
        |╩═╝╚═╝╚═╝╚═╝╩ ╩╩ ╩   ╚═╝╚═╝╚═╝
        |
        |This is the Lucuma SSO service, which also has a few utility commands that must be invoked
        |here because they cannot appear in the public API.
        |
        |Configuration is entirely by environment variable or by Java system property.
        |""".stripMargin,
) {

  import MainArgs.*

  def main: Opts[IO[ExitCode]] =
    command

  implicit val logger: Logger[IO] =
    Slf4jLogger.getLoggerFromName("lucuma-sso")

  lazy val serve =
    Command(
      name   = "serve",
      header = "Run the SSO service.",
    )((ResetDatabase.opt, SkipMigration.opt).tupled.map { case (reset, skipMigration) =>
      for
        _ <- IO.whenA(reset.isRequested)(IO.println("Resetting database."))
        _ <- IO.whenA(skipMigration.isRequested)(IO.println("Skipping migration.  Ensure that your database is up-to-date."))
        e <- FMain.serve(reset, skipMigration)
      yield e
    })

  lazy val createServiceUser =
    Command(
      name   = "create-service-user",
      header = "Create a new service user."
    )(Opts.argument[String](metavar = "name").map(FMain.createServiceUser[IO](_)))

  given Argument[StandardRole.Id] =
    Argument.from("role-id"): s =>
      Gid[StandardRole.Id].fromString.getOption(s) match
        case Some(id) => Validated.valid(id)
        case None     => Validated.invalidNel(s"Not a valid role id: $s")

  lazy val createJwt =
    Command(
      name   = "create-jwt",
      header = "Create and return a JWT for an existing user role, valid for one hour."
    )(Opts.argument[StandardRole.Id]().map(FMain.createJwt[IO](_)))

  lazy val command =
    Opts.subcommands(
      serve,
      createServiceUser,
      createJwt,
    )

}

object FMain extends AnsiColor {

  val host: Host =
    Host.fromString("0.0.0.0").getOrElse(sys.error("unpossible: invalid host"))

  // TODO: put this in the config
  val MaxConnections = 10

  // The name we're know by in the tracing back end.
  val ServiceName = "lucuma-sso"

  /** A resource that yields a Skunk session pool. */
  def databasePoolResource[F[_]: Temporal: Trace: Network: Console](
    config: DatabaseConfig
  ): Resource[F, Resource[F, Session[F]]] =
    Session.pooled(
      host     = config.host,
      port     = config.port,
      user     = config.user,
      password = config.password,
      database = config.database,
      ssl      = SSL.Trusted.withFallback(true),
      max      = MaxConnections,
      strategy = Strategy.SearchPath,
      // debug    = true,
    )


  /** A resource that yields a running HTTP server. */
  def serverResource[F[_]: Async](
    port: Port,
    app:  WebSocketBuilder2[F] => HttpApp[F]
  ): Resource[F, Server] =
    BlazeServerBuilder
      .apply[F]
      .bindHttp(port.value, "0.0.0.0")
      .withHttpWebSocketApp(app)
      .resource
    // EmberServerBuilder
    //   .default[F]
    //   .withHost(host)
    //   .withHttpWebSocketApp(app)
    //   .withPort(port)
    //   .withErrorHandler { t =>
    //     // TODO: don't show this in production
    //     val baos = new ByteArrayOutputStream
    //     val fs   = new AnsiFilterStream(baos)
    //     val osw  = new OutputStreamWriter(fs, "UTF-8")
    //     val pw   = new PrintWriter(osw)
    //     t.printStackTrace(pw)
    //     pw.close()
    //     osw.close()
    //     fs.close()
    //     baos.close()
    //     Response[F](
    //       status = Status.InternalServerError,
    //     ).withEntity(new String(baos.toByteArray, "UTF-8")).pure[F]
    //   }
    //   .build

  /**
   * A resource that yields a Natchez tracing entry point, either a Honeycomb endpoint if `config`
   * is defined, otherwise a log endpoint.
   */
  def entryPointResource[F[_]: Sync: Logger](config: Option[HoneycombConfig]): Resource[F, EntryPoint[F]] =
    config.fold(Log.entryPoint(ServiceName).pure[Resource[F, *]]) { cfg =>
      Honeycomb.entryPoint(ServiceName) { cb =>
          Sync[F].delay {
            cb.setWriteKey(cfg.writeKey)
            cb.setDataset(cfg.dataset)
            cb.build()
          }
        }
    }

  /** A resource that yields an OrcidService. */
  def orcidServiceResource[F[_]: Async: Trace: Network](config: OrcidConfig) =
    EmberClientBuilder.default[F].build.map(org.http4s.client.middleware.Logger[F](
      logHeaders = true,
      logBody    = true,
    )).map { client =>
      OrcidService(config.orcidHost, config.clientId, config.clientSecret, client)
    }

  /** A resource that yields our HttpRoutes, wrapped in accessory middleware. */
  def routesResource[F[_]: Async: Trace: Logger: Network: Console](config: Config): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    for {
      pool     <- databasePoolResource[F](config.database)
      channels <- SsoMapping.Channels(pool)
      orcid    <- orcidServiceResource(config.orcid)
    } yield wsb => ServerMiddleware[F](config).apply {
      val dbPool = pool.map(Database.fromSession(_))
      val localClient = LocalSsoClient(config.ssoJwtReader, dbPool).collect:
        case su: StandardUser => su
      Routes[F](
        dbPool    = dbPool,
        orcid     = orcid,
        jwtReader = config.ssoJwtReader,
        jwtWriter = config.ssoJwtWriter,
        publicUri = config.publicUri,
        cookies   = CookieService[F](config.cookieDomain, config.scheme === Scheme.https),
      ) <+>
      GraphQLRoutes(localClient, pool, channels, SkunkMonitor.noopMonitor[F], wsb)
    }

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] = {
    val banner =
        s"""|
            |╦  ╦ ╦╔═╗╦ ╦╔╦╗╔═╗   ╔═╗╔═╗╔═╗
            |║  ║ ║║  ║ ║║║║╠═╣───╚═╗╚═╗║ ║
            |╩═╝╚═╝╚═╝╚═╝╩ ╩╩ ╩   ╚═╝╚═╝╚═╝
            |${config.versionText}
            |${config.environment} Environment at ${config.publicUri}
            |
            |Cookie domain is ${config.cookieDomain}
            |ORCID host is ${config.orcid.orcidHost}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))
  }

  /** A startup action that runs database migrations using Flyway. */
  def migrateDatabase[F[_]: Sync](config: DatabaseConfig): F[MigrateResult] =
    Sync[F].delay {
      Flyway
        .configure()
        .dataSource(config.jdbcUrl, config.user, config.password.orEmpty)
        .baselineOnMigrate(true)
        .load()
        .migrate()
    }

  implicit def kleisliLogger[F[_]: Logger, A]: Logger[Kleisli[F, A, *]] =
    Logger[F].mapK(Kleisli.liftK)

  def singleSession[F[_]: Async: Console: Network](
    config:   DatabaseConfig,
    database: Option[String] = None
  ): Resource[F, Session[F]] =
    import natchez.Trace.Implicits.noop
    Session.single[F](
      host     = config.host,
      port     = config.port,
      user     = config.user,
      database = database.getOrElse(config.database),
      password = config.password,
      ssl      = SSL.Trusted.withFallback(true),
      strategy = Strategy.SearchPath
    )

  def resetDatabase[F[_]: Async : Console : Network](config: DatabaseConfig): F[Unit] =
    import skunk.*
    import skunk.implicits.*

    val drop   = sql"""DROP DATABASE "#${config.database}"""".command
    val create = sql"""CREATE DATABASE "#${config.database}"""".command

    singleSession(config, "postgres".some).use: s =>
      for
        _ <- s.execute(drop).void
        _ <- s.execute(create).void
      yield()

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down
   * in cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server(
    reset:         MainArgs.ResetDatabase,
    skipMigration: MainArgs.SkipMigration
  )(using Logger[IO]): Resource[IO, ExitCode] =
    for
      c  <- Resource.eval(Config.config.load[IO])
      _  <- Resource.eval(banner[IO](c))
      _  <- Applicative[Resource[IO, *]].whenA(reset.isRequested)(Resource.eval(resetDatabase[IO](c.database)))
      _  <- Applicative[Resource[IO, *]].unlessA(skipMigration.isRequested)(Resource.eval(migrateDatabase[IO](c.database)))
      ep <- entryPointResource[IO](c.honeycomb)
      ap <- ep.wsLiftR(routesResource(c)).map(_.map(_.orNotFound))
      _  <- serverResource(c.httpPort, ap)
    yield ExitCode.Success

  /** Our main server, which runs forever. */
  def serve(
    reset:         MainArgs.ResetDatabase,
    skipMigration: MainArgs.SkipMigration
  )(using Logger[IO]): IO[ExitCode] =
    server(reset, skipMigration).useForever

  /** Standalone single-use database instance for one-off commands. */
  def standaloneDatabase[F[_]: Temporal: Network: Console](
    config: DatabaseConfig
  ): Resource[F, Database[F]] = {
    import Trace.Implicits.noop
    databasePoolResource(config).flatten.map(Database.fromSession(_))
  }

  /** A one-off command that creates a service user. */
  def createServiceUser[F[_]: Async: Network: Console](
    name: String
  ): F[ExitCode] =
    Config.config.load[F].flatMap { config =>
      standaloneDatabase(config.database).use { db =>
        for {
          user <- db.canonicalizeServiceUser(name)
          _    <- Sync[F].delay(println())
          _    <- Sync[F].delay(println(s"⚠️  JWT for service user '${user.name}' (${user.id}) is valid for 20 years."))
          _    <- Sync[F].delay(println(s"⚠️  Place this value in your service configuration and do not replicate it elsewhere."))
          _    <- Sync[F].delay(println(s"⚠️  If it is lost you may re-run this command with the same service name."))
          _    <- Sync[F].delay(println())
          jwt  <- config.ssoJwtWriter.newJwt(user, Some((365 * 20).days)) // 20 years should be enough
          _    <- Sync[F].delay(println(s"${GREEN}$jwt${RESET}"))
          _    <- Sync[F].delay(println())
        } yield ExitCode.Success
      }
    }

  def createJwt[F[_]: Async: Network: Console](
    roleId: StandardRole.Id
  ): F[ExitCode] =
    Config.config.load[F].flatMap: config =>
      standaloneDatabase(config.database).use: db =>
        for
          tok <- db.createStandardUserSessionToken(roleId)
          usr <- db.getStandardUserFromToken(tok)
          jwt <- config.ssoJwtWriter.newJwt(usr, Some(1.hour))
          _   <- Console[F].println("")
          _   <- Console[F].println(s"⚠️  JWT for user '${usr.profile.displayName}' (${usr.id}, role $roleId) is valid for 1 hour.")
          _   <- Console[F].println("")
          _   <- Console[F].println(jwt)
        yield ExitCode.Success

}

