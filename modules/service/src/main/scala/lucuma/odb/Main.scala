// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats._
import cats.data.Kleisli
import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import com.comcast.ip4s.Port
import edu.gemini.grackle.skunk.SkunkMonitor
import eu.timepit.refined.auto._
import fs2.io.net.Network
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.GraphQLRoutes
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.UserService
import lucuma.sso.client.SsoClient
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import natchez.http4s.implicits._
import org.flywaydb.core.Flyway
import org.flywaydb.core.api.output.MigrateResult
import org.http4s._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.server._
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import skunk.{Command => _, _}

import scala.concurrent.duration._

object Main extends IOApp {

  // TODO: put this in the config
  val MaxConnections = 10

  // The name we're know by in the tracing back end.
  val ServiceName = "lucuma-odb"

  // Time GraphQL service instances are cached
  val GraphQLServiceTTL = 30.minutes

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] = {
    val banner =
        s"""|
            |██╗     ██╗   ██╗ ██████╗██╗   ██╗███╗   ███╗ █████╗      ██████╗ ██████╗ ██████╗
            |██║     ██║   ██║██╔════╝██║   ██║████╗ ████║██╔══██╗    ██╔═══██╗██╔══██╗██╔══██╗
            |██║     ██║   ██║██║     ██║   ██║██╔████╔██║███████║    ██║   ██║██║  ██║██████╔╝
            |██║     ██║   ██║██║     ██║   ██║██║╚██╔╝██║██╔══██║    ██║   ██║██║  ██║██╔══██╗
            |███████╗╚██████╔╝╚██████╗╚██████╔╝██║ ╚═╝ ██║██║  ██║    ╚██████╔╝██████╔╝██████╔╝
            |╚══════╝ ╚═════╝  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝  ╚═╝     ╚═════╝ ╚═════╝ ╚═════╝
            |
            |This is the Lucuma observing database.
            |
            |CommitHash.: ${config.commitHash.format}
            |CORS domain: ${config.domain}
            |ITC Root...: ${config.itcRoot}
            |Port.......: ${config.port}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))
  }

  /** A resource that yields a Skunk session pool. */
  def databasePoolResource[F[_]: Temporal: Trace: Network: Console](
    config: Config.Database
  ): Resource[F, Resource[F, Session[F]]] =
    Session.pooled(
      host     = config.host,
      port     = config.port,
      user     = config.user,
      password = Some(config.password),
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

  /** A resource that yields a Natchez tracing entry point. */
  def entryPointResource[F[_]: Sync](config: Config): Resource[F, EntryPoint[F]] =
    Honeycomb.entryPoint(ServiceName) { cb =>
      Sync[F].delay {
        cb.setWriteKey(config.honeycomb.writeKey)
        cb.setDataset(config.honeycomb.dataset)
        cb.build()
      }
    }

  /** A resource that yields our HttpRoutes, wrapped in accessory middleware. */
  def routesResource[F[_]: Async: Trace: Logger: Network: Console](
    config: Config
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    routesResource(config.database, config.itcClient, config.commitHash, config.ssoClient, config.domain)

  /** A resource that yields our HttpRoutes, wrapped in accessory middleware. */
  def routesResource[F[_]: Async: Trace: Logger: Network: Console](
    databaseConfig:    Config.Database,
    itcClientResource: Resource[F, ItcClient[F]],
    commitHash:        CommitHash,
    ssoClientResource: Resource[F, SsoClient[F, User]],
    domain:            String,
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    for {
      pool       <- databasePoolResource[F](databaseConfig)
      itcClient  <- itcClientResource
      ssoClient  <- ssoClientResource
      userSvc    <- pool.map(UserService.fromSession(_))
      middleware <- Resource.eval(ServerMiddleware(domain, ssoClient, userSvc))
      routes     <- GraphQLRoutes(itcClient, commitHash, ssoClient, pool, SkunkMonitor.noopMonitor[F], GraphQLServiceTTL, userSvc)
    } yield { wsb =>
      middleware(routes(wsb))
    }

  /** A startup action that runs database migrations using Flyway. */
  def migrateDatabase[F[_]: Sync](config: Config.Database): F[MigrateResult] =
    Sync[F].delay {
      Flyway
        .configure()
        .dataSource(config.jdbcUrl, config.user, config.password)
        .baselineOnMigrate(true)
        .load()
        .migrate()
    }

  def resetDatabase[F[_]: Async : Console](config: Config.Database): F[Unit] = {

    import skunk.*
    import skunk.implicits.*
    import natchez.Trace.Implicits.noop

    val session: Resource[F, Session[F]] =
      Session.single[F](
        host     = config.host,
        port     = config.port,
        user     = config.user,
        database = "postgres",
        password = config.password.some
      )

    val drop   = sql"""DROP DATABASE "#${config.database}"""".command
    val create = sql"""CREATE DATABASE "#${config.database}"""".command

    session.use { s =>
      for {
        _ <- s.execute(drop).void
        _ <- s.execute(create).void
      } yield()
    }

  }

  implicit def kleisliLogger[F[_]: Logger, A]: Logger[Kleisli[F, A, *]] =
    Logger[F].mapK(Kleisli.liftK)

  object Arg {
    opaque type ResetDatabase = Boolean

    object ResetDatabase {

      def apply(args: List[String]): ResetDatabase =
        args.contains("-reset")

      extension (rd: ResetDatabase) {
        def toBoolean: Boolean =
          rd

        def isRequested: Boolean =
          toBoolean
      }
    }


    opaque type SkipMigration = Boolean

    object SkipMigration {

      def apply(args: List[String]): SkipMigration =
        args.contains("-skip-migration")

      extension (sm: SkipMigration) {
        def toBoolean: Boolean =
          sm

        def isRequested: Boolean =
          toBoolean
      }
    }

  }

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down
   * in cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server[F[_]: Async: Logger: Console](
    reset:         Arg.ResetDatabase,
    skipMigration: Arg.SkipMigration
  ): Resource[F, ExitCode] =
    for {
      c  <- Resource.eval(Config.fromCiris.load[F])
      _  <- Resource.eval(banner[F](c))
      _  <- Applicative[Resource[F, *]].whenA(reset.isRequested)(Resource.eval(resetDatabase[F](c.database)))
      _  <- Applicative[Resource[F, *]].unlessA(skipMigration.isRequested)(Resource.eval(migrateDatabase[F](c.database)))
      ep <- entryPointResource(c)
      ap <- ep.wsLiftR(routesResource(c)).map(_.map(_.orNotFound))
      _  <- serverResource(c.port, ap)
    } yield ExitCode.Success

  /** Our logical entry point. */
  def runF[F[_]: Async: Logger: Console](
    reset:         Arg.ResetDatabase,
    skipMigration: Arg.SkipMigration
  ): F[ExitCode] =
    server(reset, skipMigration).use(_ => Concurrent[F].never[ExitCode])

  /** Our actual entry point. */
  def run(args: List[String]): IO[ExitCode] = {
    implicit val log: SelfAwareStructuredLogger[IO] =
      Slf4jLogger.getLoggerFromName[IO]("lucuma-odb")

    val reset         = Arg.ResetDatabase(args)
    val skipMigration = Arg.SkipMigration(args)

    for {
      _ <- IO.whenA(reset.isRequested)(IO.println("Resetting database."))
      _ <- IO.whenA(skipMigration.isRequested)(IO.println("Skipping migration.  Ensure that your database is up-to-date."))
      e <- runF[IO](reset, skipMigration)
    } yield e
  }

}

