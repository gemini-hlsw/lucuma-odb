// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.calibrations

import cats.*
import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.SecureRandom
import cats.effect.std.Supervisor
import cats.effect.std.UUIDGen
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import fs2.compression.Compression
import fs2.concurrent.Topic
import fs2.io.net.Network
import grackle.Result
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.model.Access
import lucuma.core.model.User
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.topic.CalibTimeTopic
import lucuma.odb.graphql.topic.CalibrationCalcTopic
import lucuma.odb.graphql.topic.TelluricTargetTopic
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.CalibrationCalcDaemon
import lucuma.odb.service.HminBrightnessCache
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.TelluricTargetsDaemon
import lucuma.odb.service.TelluricTargetsService
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

import scala.concurrent.duration.*

sealed trait MainParams {
  val ServiceName: String =
    "calibrations-service"

  val Header: String =
    s"""|
 ██████╗ █████╗ ██╗     ██╗██████╗ ██████╗  █████╗ ████████╗██╗ ██████╗ ███╗   ██╗███████╗
██╔════╝██╔══██╗██║     ██║██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
██║     ███████║██║     ██║██████╔╝██████╔╝███████║   ██║   ██║██║   ██║██╔██╗ ██║███████╗
██║     ██╔══██║██║     ██║██╔══██╗██╔══██╗██╔══██║   ██║   ██║██║   ██║██║╚██╗██║╚════██║
╚██████╗██║  ██║███████╗██║██████╔╝██║  ██║██║  ██║   ██║   ██║╚██████╔╝██║ ╚████║███████║
 ╚═════╝╚═╝  ╚═╝╚══════╝╚═╝╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝
        |This is the Lucuma calibrations service.
        |""".stripMargin
}

object MainParams extends MainParams

// No flags yet but we may need them in the future
object CalibrationsMain extends CommandIOApp(
  name   = MainParams.ServiceName,
  header = MainParams.Header
) {

  override def main: Opts[IO[ExitCode]] =
    Opts(serve)

  lazy val serve: IO[ExitCode] = {
    given LF: LoggerFactory[IO] = Slf4jFactory.create[IO]
    given Logger[IO] = LF.getLoggerFromName("calibrations-service")

    CMain.runF
  }

}

object CMain extends MainParams {

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] =
    val banner =
        s"""|
            |$Header
            |
            |CommitHash. : ${config.commitHash.format}
            |PID         : ${ProcessHandle.current.pid}
            |Tracing     : ${OdbTelemetry.tracingBackend(config)}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))

  /** A resource that yields a Skunk session pool. */
  def databasePoolResource[F[_]: Temporal: Tracer: Meter: Network: Console](
    config: Config.Database
  ): Resource[F, Resource[F, Session[F]]] =
    Session.Builder[F]
      .withHost(config.host)
      .withPort(config.port)
      .withUserAndPassword(config.user, config.password)
      .withDatabase(config.database)
      .withSSL(SSL.Trusted.withFallback(true))
      .withTypingStrategy(TypingStrategy.SearchPath)
      // .withDebug(true)
      // Tag connections so they can be attributed per service in
      // pg_stat_activity; see also the pool sizes in Config.Database.
      .withConnectionParameters(
        Session.DefaultConnectionParameters + ("application_name" -> "odb-calibration")
      )
      .pooled(max = config.maxCalibrationConnections)

  def serviceUser[F[_]: Async: Trace: Network: Logger](c: Config): F[Option[User]] =
    c.ssoClient.use: sso =>
      sso.get(Authorization(Credentials.Token(CIString("Bearer"), c.serviceJwt)))

  def topics[F[_]: Concurrent: Logger: Tracer](pool: Resource[F, Session[F]]):
   Resource[F, (Topic[F, CalibrationCalcTopic.Element], Topic[F, CalibTimeTopic.Element], Topic[F, TelluricTargetTopic.Element])] =
    for {
      sup <- Supervisor[F]
      ses <- pool
      ctt <- Resource.eval(CalibTimeTopic(ses, 1024, sup))
      cct <- Resource.eval(CalibrationCalcTopic(ses, 1024, sup))
      trt <- Resource.eval(TelluricTargetTopic(ses, 1024, sup))
    } yield (cct, ctt, trt)

  // Drains the durable `t_calibration_calc` queue
  def runCalibrationCalcDaemon[F[_]: {Async, LoggerFactory as LF, Tracer as T}](
    calcTopic:        Topic[F, CalibrationCalcTopic.Element],
    connectionsLimit: Int,
    pollPeriod:       FiniteDuration,
    services:         Resource[F, Services[F]]
  ): Resource[F, Unit] =
    CalibrationCalcDaemon.run(
      connectionsLimit = connectionsLimit,
      pollPeriod       = pollPeriod,
      batchSize        = 10,
      topic            = calcTopic,
      services         = services
    )

  // Calibration-time still uses NOTIFY-only.
  // TODO:. We should consider moving it to a durable queue as well
  def runCalibTimeDaemon[F[_]: {Async, Tracer as T, Logger as L}](
    calibTopic: Topic[F, CalibTimeTopic.Element],
    services:   Resource[F, Services[F]]
  ): Resource[F, Unit] =
    for {
      _ <- Resource.eval(info"Calibrations Service starting")
      _ <- Resource.eval(info"Start listening for calibration time changes")
      _ <- Resource.eval(calibTopic.subscribe(100).evalMap: elem =>
             T.rootSpan("calibration-time-event").surround:
               services.useTransactionally:
                 Services.asSuperUser:
                   calibrationsService.recalculateCalibrationTarget(elem.programId, elem.observationId)
                     .map(Result.success)
             .handleErrorWith: e =>
               L.error(e)(
                 s"Error processing calib time event for ${elem.programId}/${elem.observationId}"
               ).as(Result.unit)
           .compile.drain.start.void)
    } yield ()

  def runTelluricTargetsDaemon[F[_]: {Async, Parallel, Logger, LoggerFactory, Tracer}](
    connectionsLimit: Int,
    pollPeriod: FiniteDuration,
    telluricTopic: Topic[F, TelluricTargetTopic.Element],
    services: Resource[F, Services[F]]
  ): Resource[F, Unit] =
    Resource.eval:
      info"Telluric Resolution Daemon starting" *>
        TelluricTargetsDaemon.run(
          connectionsLimit = connectionsLimit,
          pollPeriod = pollPeriod,
          batchSize = 10,
          topic = telluricTopic,
          services = services
        )

  def services[F[_]: Async: Parallel: UUIDGen: Tracer: Logger: LoggerFactory](
    user: Option[User],
    emailConfig: Config.Email,
    commitHash: CommitHash,
    calculator: TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient: Client[F],
    itcClient: ItcClient[F],
    gaiaClient: GaiaClient[F],
    horizonsClient: HorizonsClient[F],
    telClient: TelluricTargetsClient[F],
    hminCache: HminBrightnessCache,
  )(pool: Session[F]): F[Services[F]] =
    user match {
      case Some(u) if u.role.access === Access.Service =>
        Services.forUser(
          u,
          None,
          emailConfig,
          commitHash,
          calculator,
          httpClient,
          itcClient,
          gaiaClient,
          S3FileService.noop[F],
          horizonsClient,
          telClient,
          GoaClient.noop[F],
          hminCache
        )(pool).pure
      case Some(u) =>
        Logger[F].error(s"User $u is not allowed to execute this service") *>
          MonadThrow[F].raiseError(new RuntimeException(s"User $u doesn't have permission to execute"))
      case None    =>
        Logger[F].error("Failed to get service user") *>
          MonadThrow[F].raiseError(new RuntimeException("Failed to get service user"))
    }

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down
   * in cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server[F[_]: Async: Compression: Parallel: Logger: LoggerFactory: Trace: Tracer: TracerProvider: Meter: Console: Network: SecureRandom]: Resource[F, ExitCode] =
    for {
      c                  <- Resource.eval(Config.fromCiris.load[F])
      _                  <- Resource.eval(banner[F](c))
      pool               <- databasePoolResource[F](c.database)
      enums              <- Resource.eval(pool.use(Enums.load))
      (ccT, ctT, trT)    <- topics(pool)
      user               <- Resource.eval(serviceUser[F](c))
      _                  <- Resource.eval(user.traverse_ : u =>
                              pool.use(s => Services.asSuperUser(UserService.fromSession(s).canonicalizeUser(u)))
                            )
      httpClient         <- c.httpClientResource
      gaiaClient         <- c.gaiaClient
      horizonsClient     <- c.horizonsClientResource
      telClient          <- c.telluricClient
      ptc                <- Resource.eval(pool.use(TimeEstimateCalculatorImplementation.fromSession(_, enums)))
      itcClient          <- c.itcClient
      hminCache          <- Resource.eval(pool.use(TelluricTargetsService.loadBrightnessCache))
      _                  <- Resource.eval(info"Loading ${hminCache.value.size} configurations for telluric brightness")
      servicesResource   = pool.evalMap(services(user, c.email, c.commitHash, ptc, httpClient, itcClient, gaiaClient, horizonsClient, telClient, hminCache))
      _                  <- runCalibrationCalcDaemon(ccT, c.database.maxObscalcConnections, c.obscalcPoll, servicesResource)
      _                  <- runCalibTimeDaemon(ctT, servicesResource)
      _                  <- runTelluricTargetsDaemon(c.database.maxObscalcConnections, c.obscalcPoll, trT, servicesResource)
    } yield ExitCode.Success

  /** Our logical entry point. */
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
