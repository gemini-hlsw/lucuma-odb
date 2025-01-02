// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.*
import cats.data.Kleisli
import cats.effect.*
import cats.effect.std.AtomicCell
import cats.effect.std.Console
import cats.implicits.*
import com.comcast.ip4s.Port
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import fs2.io.net.Network
import grackle.skunk.SkunkMonitor
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.core.model.User
import lucuma.graphql.routes.GraphQLService
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.AttachmentRoutes
import lucuma.odb.graphql.EmailWebhookRoutes
import lucuma.odb.graphql.GraphQLRoutes
import lucuma.odb.graphql.OdbMapping
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.EmailWebhookService
import lucuma.odb.service.ItcService
import lucuma.odb.service.S3FileService
import lucuma.odb.service.UserService
import lucuma.sso.client.SsoClient
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import natchez.http4s.implicits.*
import org.flywaydb.core.Flyway
import org.flywaydb.core.api.output.MigrateResult
import org.http4s.*
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.client.Client
import org.http4s.implicits.*
import org.http4s.server.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import skunk.{Command as _, *}
import software.amazon.awssdk.services.s3.presigner.S3Presigner

import scala.concurrent.duration.*

object MainArgs {
  opaque type ResetDatabase = Boolean

  object ResetDatabase {

    val opt: Opts[ResetDatabase] =
      Opts.flag("reset", help = "Drop and recreate the database before starting.").orFalse

    extension (rd: ResetDatabase) {
      def toBoolean: Boolean =
        rd

      def isRequested: Boolean =
        toBoolean
    }
  }


  opaque type SkipMigration = Boolean

  object SkipMigration {

    val opt: Opts[SkipMigration] =
      Opts.flag("skip-migration", help = "Skip database migration on startup.").orFalse

    extension (sm: SkipMigration) {
      def toBoolean: Boolean =
        sm

      def isRequested: Boolean =
        toBoolean
    }
  }
}

sealed trait MainParams {
  val ServiceName: String =
    "lucuma-odb"

  val Header: String =
    s"""|██╗     ██╗   ██╗ ██████╗██╗   ██╗███╗   ███╗ █████╗      ██████╗ ██████╗ ██████╗
        |██║     ██║   ██║██╔════╝██║   ██║████╗ ████║██╔══██╗    ██╔═══██╗██╔══██╗██╔══██╗
        |██║     ██║   ██║██║     ██║   ██║██╔████╔██║███████║    ██║   ██║██║  ██║██████╔╝
        |██║     ██║   ██║██║     ██║   ██║██║╚██╔╝██║██╔══██║    ██║   ██║██║  ██║██╔══██╗
        |███████╗╚██████╔╝╚██████╗╚██████╔╝██║ ╚═╝ ██║██║  ██║    ╚██████╔╝██████╔╝██████╔╝
        |╚══════╝ ╚═════╝  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝  ╚═╝     ╚═════╝ ╚═════╝ ╚═════╝
        |
        |This is the Lucuma observing database.
        |""".stripMargin
}

object MainParams extends MainParams


object Main extends CommandIOApp(
  name   = MainParams.ServiceName,
  header = MainParams.Header
) {

  import MainArgs.*

  override def main: Opts[IO[ExitCode]] =
    command

  lazy val serve: Command[IO[ExitCode]] =
    Command(
      name    = "serve",
      header  = "Run the ODB service.",
    )((ResetDatabase.opt, SkipMigration.opt).tupled.map { case (reset, skipMigration) =>
      given Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("lucuma-odb")

      for {
        _ <- IO.whenA(reset.isRequested)(IO.println("Resetting database."))
        _ <- IO.whenA(skipMigration.isRequested)(IO.println("Skipping migration.  Ensure that your database is up-to-date."))
        e <- FMain.runF[IO](reset, skipMigration)
      } yield e
    })

  lazy val command: Opts[IO[ExitCode]] =
    Opts.subcommands(
      serve
    )
}

object FMain extends MainParams {

  import MainArgs.*

  // Time GraphQL service instances are cached
  val GraphQLServiceTTL = 30.minutes

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] = {
    val banner =
        s"""|
            |$Header
            |
            |CommitHash. : ${config.commitHash.format}
            |CORS domains: ${config.domain.mkString(", ")}
            |ITC Root    : ${config.itc.root}
            |Port        : ${config.port}
            |PID         : ${ProcessHandle.current.pid}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))
  }

  /** A resource that yields a Skunk session pool. */
  def databasePoolResource[F[_]: Temporal: Trace: Network: Console: Logger](
    config: Config.Database
  ): Resource[F, Resource[F, Session[F]]] =
    Resource.eval(AtomicCell[F].of(config.maxConnections)).flatMap: cell =>
      Session.pooled(
        host     = config.host,
        port     = config.port,
        user     = config.user,
        password = Some(config.password),
        database = config.database,
        ssl      = SSL.Trusted.withFallback(true),
        max      = config.maxConnections,
        strategy = Strategy.SearchPath,
        // debug    = true,
      ).map: rsrc =>
        rsrc
          .preAllocate(cell.updateAndGet(_ - 1).flatMap(n => Logger[F].debug(s"acquiring session (should be $n remaining)")))
          .onFinalize(cell.updateAndGet(_ + 1).flatMap(n => Logger[F].debug(s"released session (should be $n remaining)")))

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
  def routesResource[F[_]: Async: Parallel: Trace: Logger: Network: Console](
    config: Config
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    routesResource(
      config.database,
      config.aws,
      config.email,
      config.itcClient,
      config.commitHash,
      config.ssoClient,
      config.corsOverHttps,
      config.domain,
      S3FileService.s3AsyncClientOpsResource(config.aws),
      S3FileService.s3PresignerResource(config.aws),
      config.httpClientResource
    )

  /** A resource that yields our HttpRoutes, wrapped in accessory middleware. */
  def routesResource[F[_]: Async: Parallel: Trace: Logger: Network: Console](
    databaseConfig:       Config.Database,
    awsConfig:            Config.Aws,
    emailConfig:          Config.Email,
    itcClientResource:    Resource[F, ItcClient[F]],
    commitHash:           CommitHash,
    ssoClientResource:    Resource[F, SsoClient[F, User]],
    corsOverHttps:        Boolean,
    domain:               List[String],
    s3OpsResource:        Resource[F, S3AsyncClientOp[F]],
    s3PresignerResource:  Resource[F, S3Presigner],
    httpClientResource:   Resource[F, Client[F]]
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    for {
      pool              <- databasePoolResource[F](databaseConfig)
      itcClient         <- itcClientResource
      ssoClient         <- ssoClientResource
      httpClient        <- httpClientResource
      userSvc           <- pool.map(UserService.fromSession(_))
      middleware        <- Resource.eval(ServerMiddleware(corsOverHttps, domain, ssoClient, userSvc))
      enums             <- Resource.eval(pool.use(Enums.load))
      ptc               <- Resource.eval(pool.use(TimeEstimateCalculatorImplementation.fromSession(_, enums)))
      graphQLRoutes     <- GraphQLRoutes(itcClient, commitHash, ssoClient, pool, SkunkMonitor.noopMonitor[F], GraphQLServiceTTL, userSvc, enums, ptc, httpClient, emailConfig)
      s3ClientOps       <- s3OpsResource
      s3Presigner       <- s3PresignerResource
      s3FileService      = S3FileService.fromS3ConfigAndClient(awsConfig, s3ClientOps, s3Presigner)
      metadataService    = GraphQLService(OdbMapping.forMetadata(pool, SkunkMonitor.noopMonitor[F], enums))
      webhookService    <- pool.map(EmailWebhookService.fromSession(_))
    } yield { wsb =>
      val attachmentRoutes =  AttachmentRoutes.apply[F](pool, s3FileService, ssoClient, enums, awsConfig.fileUploadMaxMb)
      val metadataRoutes = GraphQLRoutes.enumMetadata(metadataService)
      val emailWebhookRoutes = EmailWebhookRoutes(webhookService, emailConfig)
      middleware(graphQLRoutes(wsb) <+> attachmentRoutes <+>  metadataRoutes <+> emailWebhookRoutes)
    }

  /** A startup action that runs database migrations using Flyway. */
  def migrateDatabase[F[_]: Sync](config: Config.Database): F[MigrateResult] =
    Sync[F].delay {
      Flyway
        .configure()
        .loggers("slf4j")
        .dataSource(config.jdbcUrl, config.user, config.password)
        .baselineOnMigrate(true)
        .load()
        .migrate()
    }

  def singleSession[F[_]: Async: Console: Network](
    config:   Config.Database,
    database: Option[String] = None
  ): Resource[F, Session[F]] = {

    import natchez.Trace.Implicits.noop

    Session.single[F](
      host     = config.host,
      port     = config.port,
      user     = config.user,
      database = database.getOrElse(config.database),
      password = config.password.some,
      ssl      = SSL.Trusted.withFallback(true),
      strategy = Strategy.SearchPath
    )
  }

  def resetDatabase[F[_]: Async : Console : Network](config: Config.Database): F[Unit] = {

    import skunk.*
    import skunk.implicits.*

    val drop   = sql"""DROP DATABASE "#${config.database}"""".command
    val create = sql"""CREATE DATABASE "#${config.database}"""".command

    singleSession(config, "postgres".some).use { s =>
      for {
        _ <- s.execute(drop).void
        _ <- s.execute(create).void
      } yield()
    }
  }

  implicit def kleisliLogger[F[_]: Logger, A]: Logger[Kleisli[F, A, *]] =
    Logger[F].mapK(Kleisli.liftK)

  // This derivation is required to avoid a deprecation warning in the call to wsLiftR below.
  given [F[_]: Async, A]: Network[Kleisli[F, A, _]] = Network.forAsync

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down
   * in cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server[F[_]: Async: Parallel: Logger: Console: Network](
    reset:         ResetDatabase,
    skipMigration: SkipMigration
  ): Resource[F, ExitCode] =
    for {
      c  <- Resource.eval(Config.fromCiris.load[F])
      _  <- Resource.eval(banner[F](c))
      _  <- Applicative[Resource[F, *]].whenA(reset.isRequested)(Resource.eval(resetDatabase[F](c.database)))
      _  <- Applicative[Resource[F, *]].unlessA(skipMigration.isRequested)(Resource.eval(migrateDatabase[F](c.database)))
      ep <- entryPointResource(c)
      ap <- ep.wsLiftR(routesResource(c)).map(_.map(_.orNotFound))
      _  <- Resource.eval(ItcService.pollVersionsForever(c.itcClient, singleSession(c.database), c.itc.pollPeriod))
      _  <- serverResource(c.port, ap)
    } yield ExitCode.Success

  /** Our logical entry point. */
  def runF[F[_]: Async: Parallel: Logger: Console: Network](
    reset:         ResetDatabase,
    skipMigration: SkipMigration
  ): F[ExitCode] =
    server(reset, skipMigration).use(_ => Concurrent[F].never[ExitCode])

}

