// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats._
import cats.data.Kleisli
import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import com.comcast.ip4s.Port
import com.monovore.decline._
import com.monovore.decline.effect.CommandIOApp
import edu.gemini.grackle.skunk.SkunkMonitor
import eu.timepit.refined.auto._
import fs2.io.net.Network
import io.laserdisc.pure.s3.tagless.Interpreter
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.GraphQLRoutes
import lucuma.odb.graphql.ObsAttachmentRoutes
import lucuma.odb.graphql.ProposalAttachmentRoutes
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ObsAttachmentFileService
import lucuma.odb.service.ProposalAttachmentFileService
import lucuma.odb.service.S3FileService
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
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3AsyncClient
import software.amazon.awssdk.services.s3.S3Configuration

import scala.concurrent.duration._

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
      implicit val log: SelfAwareStructuredLogger[IO] =
        Slf4jLogger.getLoggerFromName[IO]("lucuma-odb")

      for {
        _ <- IO.whenA(reset.isRequested)(IO.println("Resetting database."))
        _ <- IO.whenA(skipMigration.isRequested)(IO.println("Skipping migration.  Ensure that your database is up-to-date."))
        e <- FMain.runF(reset, skipMigration)
      } yield e
    })

  lazy val command: Opts[IO[ExitCode]] =
    Opts.subcommands(
      serve
    )
}

object FMain extends MainParams {

  import MainArgs.*

  // TODO: put this in the config
  val MaxConnections = 10

  // Time GraphQL service instances are cached
  val GraphQLServiceTTL = 30.minutes

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] = {
    val banner =
        s"""|
            |$Header
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

  /** A resource that encapsulates an s3 client */
  def s3ClientOpsResource[F[_]: Async](awsConfig: Config.Aws): Resource[F, S3AsyncClientOp[F]] = {
    val credentials = AwsBasicCredentials.create(awsConfig.accessKey.value, awsConfig.secretKey.value)

    Interpreter[F].S3AsyncClientOpResource(
      S3AsyncClient
            .builder()
            .credentialsProvider(StaticCredentialsProvider.create(credentials))
            .serviceConfiguration(
              S3Configuration
                .builder()
                .pathStyleAccessEnabled(true)
                .build()
            )
            .region(Region.US_EAST_1)
    )
  }

  /** A resource that yields our HttpRoutes, wrapped in accessory middleware. */
  def routesResource[F[_]: Async: Trace: Logger: Network: Console](
    config: Config
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    routesResource(
      config.database,
      config.aws,
      config.itcClient,
      config.commitHash,
      config.ssoClient,
      config.domain,
      s3ClientOpsResource(config.aws)
    )

  /** A resource that yields our HttpRoutes, wrapped in accessory middleware. */
  def routesResource[F[_]: Async: Trace: Logger: Network: Console](
    databaseConfig:    Config.Database,
    awsConfig:         Config.Aws,
    itcClientResource: Resource[F, ItcClient[F]],
    commitHash:        CommitHash,
    ssoClientResource: Resource[F, SsoClient[F, User]],
    domain:            String,
    s3OpsResource:     Resource[F, S3AsyncClientOp[F]]
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    for {
      pool              <- databasePoolResource[F](databaseConfig)
      itcClient         <- itcClientResource
      ssoClient         <- ssoClientResource
      userSvc           <- pool.map(UserService.fromSession(_))
      middleware        <- Resource.eval(ServerMiddleware(domain, ssoClient, userSvc))
      enums             <- Resource.eval(pool.use(Enums.load))
      graphQLRoutes     <- GraphQLRoutes(itcClient, commitHash, ssoClient, pool, SkunkMonitor.noopMonitor[F], GraphQLServiceTTL, userSvc, enums)
      s3ClientOps       <- s3OpsResource
      s3FileService      = S3FileService.fromS3ConfigAndClient(awsConfig, s3ClientOps)
      obsAttachFileSvc  <- pool.map(ses => ObsAttachmentFileService.fromS3AndSession(s3FileService, ses))
      propAttachFileSvc <- pool.map(ses => ProposalAttachmentFileService.fromS3AndSession(s3FileService, ses))
    } yield { wsb =>
      val obsAttachmentRoutes =  ObsAttachmentRoutes.apply[F](obsAttachFileSvc, ssoClient, awsConfig.fileUploadMaxMb)
      val proposalAttachmentRoutes = ProposalAttachmentRoutes[F](propAttachFileSvc, ssoClient, awsConfig.fileUploadMaxMb)
      middleware(graphQLRoutes(wsb) <+> obsAttachmentRoutes <+> proposalAttachmentRoutes)
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

  def singleSession[F[_]: Temporal: Network: Console](
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
      ssl      = SSL.Trusted.withFallback(true)
    )
}

  def resetDatabase[F[_]: Temporal: Network: Console](config: Config.Database): F[Unit] = {

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

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down
   * in cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server(
    reset:         ResetDatabase,
    skipMigration: SkipMigration
  )(using Logger[IO]): Resource[IO, ExitCode] =
    for {
      c  <- Resource.eval(Config.fromCiris.load[IO])
      _  <- Resource.eval(banner[IO](c))
      _ <- resetDatabase[IO](c.database).whenA(reset.isRequested).toResource
      _ <- migrateDatabase[IO](c.database).unlessA(skipMigration.isRequested).toResource
      ep <- entryPointResource[IO](c)
      ap <- ep.wsLiftR(routesResource(c)).map(_.map(_.orNotFound))
      _  <- serverResource(c.port, ap)
    } yield ExitCode.Success

  /** Our logical entry point. */
  def runF(
    reset:         ResetDatabase,
    skipMigration: SkipMigration
  )(using Logger[IO]): IO[ExitCode] =
    server(reset, skipMigration).use(_ => IO.never[ExitCode])

}

