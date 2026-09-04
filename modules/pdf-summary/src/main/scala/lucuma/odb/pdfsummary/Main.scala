// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.pdfsummary

import cats.*
import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.SecureRandom
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import fs2.compression.Compression
import fs2.io.file.Files
import fs2.io.net.Network
import fs2.io.process.Processes
import grackle.Mapping
import grackle.skunk.SkunkMonitor
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.model.Access
import lucuma.core.model.User
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.OdbMapping
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.PdfRenderer
import lucuma.odb.service.PdfSummaryJobDaemon
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.odb.util.OdbTelemetry
import natchez.Trace
import org.http4s.Credentials
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider
import skunk.*

sealed trait MainParams {
  val ServiceName: String =
    "pdf-summary-service"

  val Header: String =
    s"""|
██████╗ ██████╗ ███████╗    ███████╗██╗   ██╗███╗   ███╗███╗   ███╗ █████╗ ██████╗ ██╗   ██╗
██╔══██╗██╔══██╗██╔════╝    ██╔════╝██║   ██║████╗ ████║████╗ ████║██╔══██╗██╔══██╗╚██╗ ██╔╝
██████╔╝██║  ██║█████╗      ███████╗██║   ██║██╔████╔██║██╔████╔██║███████║██████╔╝ ╚████╔╝
██╔═══╝ ██║  ██║██╔══╝      ╚════██║██║   ██║██║╚██╔╝██║██║╚██╔╝██║██╔══██║██╔══██╗  ╚██╔╝
██║     ██████╔╝██║         ███████║╚██████╔╝██║ ╚═╝ ██║██║ ╚═╝ ██║██║  ██║██║  ██║   ██║
╚═╝     ╚═════╝ ╚═╝         ╚══════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝
        |This is the Lucuma proposal-summary PDF service.
        |""".stripMargin
}

object MainParams extends MainParams

object PdfSummaryMain extends CommandIOApp(
  name   = MainParams.ServiceName,
  header = MainParams.Header
) {

  override def main: Opts[IO[ExitCode]] =
    Opts(serve)

  lazy val serve: IO[ExitCode] = {
    given LF: LoggerFactory[IO] = Slf4jFactory.create[IO]
    given Logger[IO] = LF.getLoggerFromName("pdf-summary-service")

    PMain.runF
  }

}

object PMain extends MainParams {

  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] =
    val banner =
        s"""|
            |$Header
            |
            |CommitHash. : ${config.commitHash.format}
            |PID         : ${ProcessHandle.current.pid}
            |Tracing     : ${OdbTelemetry.tracingBackend(config)}
            |Python      : ${config.pdfSummary.python}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))

  def databasePoolResource[F[_]: Temporal: Tracer: Meter: Network: Console](
    config: Config.Database,
    max:    Int
  ): Resource[F, Resource[F, Session[F]]] =
    Session.Builder[F]
      .withHost(config.host)
      .withPort(config.port)
      .withUserAndPassword(config.user, config.password)
      .withDatabase(config.database)
      .withSSL(SSL.Trusted.withFallback(true))
      .withTypingStrategy(TypingStrategy.SearchPath)
      .withConnectionParameters(
        Session.DefaultConnectionParameters + ("application_name" -> "odb-pdf-summary")
      )
      .pooled(max = max)

  def serviceUser[F[_]: Async: Trace: Network: Logger](c: Config): F[Option[User]] =
    c.ssoClient.use: sso =>
      sso.get(Authorization(Credentials.Token(CIString("Bearer"), c.serviceJwt)))

  def services[F[_]: Async: Parallel: UUIDGen: Tracer: Logger: LoggerFactory](
    user:           Option[User],
    emailConfig:    Config.Email,
    commitHash:     CommitHash,
    calculator:     TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient:     Client[F],
    itcClient:      ItcClient[F],
    gaiaClient:     GaiaClient[F],
    horizonsClient: HorizonsClient[F],
    s3FileService:  S3FileService[F],
    mapping:        User => Session[F] => Mapping[F]
  )(pool: Session[F]): F[Services[F]] =
    user match {
      case Some(u) if u.role.access === Access.Service =>
        Services.forUser(
          u,
          mapping(u).some,
          emailConfig,
          commitHash,
          calculator,
          httpClient,
          itcClient,
          gaiaClient,
          s3FileService,
          horizonsClient,
          TelluricTargetsClient.noop[F],
          GoaClient.noop[F]
        )(pool).pure
      case Some(u) =>
        Logger[F].error(s"User $u is not allowed to execute this service") *>
          MonadThrow[F].raiseError(new RuntimeException(s"User $u doesn't have permission to execute"))
      case None    =>
        Logger[F].error("Failed to get service user") *>
          MonadThrow[F].raiseError(new RuntimeException("Failed to get service user"))
    }

  def server[F[_]: Async: Compression: Files: Processes: Parallel: Logger: LoggerFactory: Trace: Tracer: TracerProvider: Meter: Console: Network: SecureRandom]: Resource[F, ExitCode] =
    for {
      c                <- Resource.eval(Config.fromCiris.load[F])
      _                <- Resource.eval(banner[F](c))
      pool             <- databasePoolResource[F](c.database, c.pdfSummary.maxConnections)
      enums            <- Resource.eval(pool.use(Enums.load))
      user             <- Resource.eval(serviceUser[F](c))
      _                <- Resource.eval(user.traverse_ : u =>
                            pool.use(s => Services.asSuperUser(UserService.fromSession(s).canonicalizeUser(u)))
                          )
      httpClient       <- c.httpClientResource
      gaiaClient       <- c.gaiaClient
      horizonsClient   <- c.horizonsClientResource
      ptc              <- Resource.eval(pool.use(TimeEstimateCalculatorImplementation.fromSession(_, enums)))
      itcClient        <- c.itcClient
      s3ClientOps      <- S3FileService.s3AsyncClientOpsResource[F](c.aws)
      s3Presigner      <- S3FileService.s3PresignerResource[F](c.aws)
      s3FileService     = S3FileService.fromS3ConfigAndClient(c.aws, s3ClientOps, s3Presigner)
      schema           <- Resource.eval(OdbMapping.loadSchema[F])
      // Payload preparation runs a GraphQL query, so the services need a mapping.
      mapping           = (u: User) => (s: Session[F]) =>
                            OdbMapping.forObscalc(
                              Resource.pure(s),
                              SkunkMonitor.noopMonitor[F],
                              u,
                              c.goaUsers,
                              gaiaClient,
                              itcClient,
                              c.commitHash,
                              ptc,
                              httpClient,
                              horizonsClient,
                              GoaClient.noop[F],
                              c.email,
                              schema
                            )
      servicesResource  = pool.evalMap(services(user, c.email, c.commitHash, ptc, httpClient, itcClient, gaiaClient, horizonsClient, s3FileService, mapping))
      renderer          = PdfRenderer.subprocess[F](c.pdfSummary.python, c.pdfSummary.renderTimeout)
      _                <- Resource.eval(info"PDF summary job daemon starting")
      _                <- PdfSummaryJobDaemon.run(c.obscalcPoll, pool, servicesResource, renderer)
    } yield ExitCode.Success

  def runF(using Logger[IO], LoggerFactory[IO]): IO[ExitCode] =
    (for
      c                        <- Resource.eval(Config.fromCiris.load[IO])
      otel                     <- OdbTelemetry.otel(ServiceName, c)
      given Tracer[IO]         = otel.tracer
      given Trace[IO]          = otel.trace
      given Meter[IO]          = otel.meter
      given TracerProvider[IO] = otel.tracerProvider
      _                        <- server[IO]
    yield ExitCode.Success).useForever

}
