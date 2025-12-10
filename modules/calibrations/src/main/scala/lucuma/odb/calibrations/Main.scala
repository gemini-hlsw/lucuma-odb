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
import fs2.concurrent.Topic
import fs2.io.net.Network
import grackle.Result
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.model.Access
import lucuma.core.model.User
import lucuma.core.util.CalculationState
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.data.EditType
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.topic.CalibTimeTopic
import lucuma.odb.graphql.topic.ObscalcTopic
import lucuma.odb.graphql.topic.TelluricTargetTopic
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.HminBrightnessCache
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.TelluricTargetsDaemon
import lucuma.odb.service.TelluricTargetsService
import lucuma.odb.service.UserService
import lucuma.odb.util.LucumaEntryPoint
import natchez.Trace
import org.http4s.Credentials
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.syntax.*
import skunk.{Command as _, *}

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
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

    import natchez.Trace.Implicits.noop

    CMain.runF[IO]
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
            |Tracing     : ${LucumaEntryPoint.tracingBackend(config)}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))

  /** A resource that yields a Skunk session pool. */
  def databasePoolResource[F[_]: Temporal: Network: Console](
    config: Config.Database
  ): Resource[F, Resource[F, Session[F]]] = {
    import natchez.Trace.Implicits.noop
    Session.pooled(
      host     = config.host,
      port     = config.port,
      user     = config.user,
      password = Some(config.password),
      database = config.database,
      ssl      = SSL.Trusted.withFallback(true),
      max      = config.maxCalibrationConnections,
      strategy = Strategy.SearchPath,
      // debug    = true,
    )
  }

  def serviceUser[F[_]: Async: Trace: Network: Logger](c: Config): F[Option[User]] =
    c.ssoClient.use: sso =>
      sso.get(Authorization(Credentials.Token(CIString("Bearer"), c.serviceJwt)))

  def topics[F[_]: Concurrent: Logger: Trace](pool: Resource[F, Session[F]]):
   Resource[F, (Topic[F, ObscalcTopic.Element], Topic[F, CalibTimeTopic.Element], Topic[F, TelluricTargetTopic.Element])] =
    for {
      sup <- Supervisor[F]
      ses <- pool
      ctt <- Resource.eval(CalibTimeTopic(ses, 1024, sup))
      top <- Resource.eval(ObscalcTopic(ses, 1024, sup))
      trt <- Resource.eval(TelluricTargetTopic(ses, 1024, sup))
    } yield (top, ctt, trt)

  def runCalibrationsDaemon[F[_]: {Async, Logger, Clock as C}](
    obscalcTopic: Topic[F, ObscalcTopic.Element],
    calibTopic: Topic[F, CalibTimeTopic.Element],
    services: Resource[F, Services[F]]
  ): Resource[F, Unit] =
    for {
      _  <- Resource.eval(info"Calibrations Service starting")
      _  <- Resource.eval(info"Start listening for obscalc changes")
      _  <- Resource.eval(obscalcTopic.subscribe(100).evalMap { elem =>
              services.use: svc =>
                services.useTransactionally:
                  Services.asSuperUser:
                    for {
                      i <- svc.calibrationsService.isCalibration(elem.observationId)
                      _ <- (info"Calibrations Service Obscalc channel: Element(${elem.observationId},${elem.programId},${elem.editType},oldState=${elem.oldState},newState=${elem.newState},${elem.users}), is calibration: $i").whenA(i)
                      t <- C.realTimeInstant.map(LocalDate.ofInstant(_, ZoneOffset.UTC))
                      _ <- calibrationsService
                            .recalculateCalibrations(
                              elem.programId,
                              LocalDateTime.of(t, LocalTime.MIDNIGHT).toInstant(ZoneOffset.UTC)
                            ).whenA(!i &&
                                    elem.newState.exists(_ === CalculationState.Ready) &&
                                    elem.oldState =!= elem.newState &&
                                    (elem.editType === EditType.Created ||
                                     elem.editType === EditType.Updated))
                    } yield Result.unit
            }.compile.drain.start.void)
      _  <- Resource.eval(info"Start listening for calibration time changes")
      _  <- Resource.eval(calibTopic.subscribe(100).evalMap: elem =>
              services.useTransactionally:
                Services.asSuperUser:
                  calibrationsService.recalculateCalibrationTarget(elem.programId, elem.observationId)
                    .map(Result.success)
            .compile.drain.start.void)
    } yield ()

  def runTelluricTargetsDaemon[F[_]: {Async, Parallel, Logger, LoggerFactory}](
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

  def services[F[_]: Temporal: Async: Parallel: UUIDGen: Trace: Logger: LoggerFactory](
    user: Option[User],
    enums: Enums,
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
          enums,
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
          hminCache
        )(pool).pure[F].flatTap { _ =>
          val us = UserService.fromSession(pool)
          Services.asSuperUser(us.canonicalizeUser(u))
        }
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
  def server[F[_]: Async: Parallel: Logger: LoggerFactory: Trace: Console: Network: SecureRandom]: Resource[F, ExitCode] =
    for {
      c                  <- Resource.eval(Config.fromCiris.load[F])
      _                  <- Resource.eval(banner[F](c))
      ep                 <- LucumaEntryPoint.entryPointResource(ServiceName, c)
      pool               <- databasePoolResource[F](c.database)
      enums              <- Resource.eval(pool.use(Enums.load))
      (obsT, ctT, trT)   <- topics(pool)
      user               <- Resource.eval(serviceUser[F](c))
      httpClient         <- c.httpClientResource
      gaiaClient         <- c.gaiaClient
      horizonsClient     <- c.horizonsClientResource
      telClient          <- c.telluricClient
      ptc                <- Resource.eval(pool.use(TimeEstimateCalculatorImplementation.fromSession(_, enums)))
      itcClient          <- c.itcClient
      hminCache          <- Resource.eval(pool.use(TelluricTargetsService.loadBrightnessCache))
      _                  <- Resource.eval(info"Loading ${hminCache.value.size} configurations for telluric brightness")
      servicesResource   = pool.evalMap(services(user, enums, c.email, c.commitHash, ptc, httpClient, itcClient, gaiaClient, horizonsClient, telClient, hminCache))
      _                  <- runCalibrationsDaemon(obsT, ctT, servicesResource)
      _                  <- runTelluricTargetsDaemon(c.database.maxObscalcConnections, c.obscalcPoll, trT, servicesResource)
    } yield ExitCode.Success

  /** Our logical entry point. */
  def runF[F[_]:   Async: Parallel: Logger: LoggerFactory: Trace: Network: Console: SecureRandom]: F[ExitCode] =
    server.use(_ => Concurrent[F].never[ExitCode])

}
