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
import cats.implicits.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import fs2.concurrent.Topic
import fs2.io.net.Network
import grackle.Result
import lucuma.core.model.Access
import lucuma.core.model.User
import lucuma.odb.Config
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.topic.CalibTimeTopic
import lucuma.odb.graphql.topic.ObservationTopic
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.UserService
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import org.http4s.Credentials
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
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
    given Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("calibrations-service")

    import natchez.Trace.Implicits.noop

    CMain.runF[IO]
  }

}

object CMain extends MainParams {

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] = {
    val banner =
        s"""|
            |$Header
            |
            |CommitHash. : ${config.commitHash.format}
            |PID         : ${ProcessHandle.current.pid}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))
  }

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


  /** A resource that yields a Natchez tracing entry point. */
  def entryPointResource[F[_]: Sync](config: Config): Resource[F, EntryPoint[F]] =
    Honeycomb.entryPoint(ServiceName) { cb =>
      Sync[F].delay {
        cb.setWriteKey(config.honeycomb.writeKey)
        cb.setDataset(config.honeycomb.dataset)
        cb.build()
      }
    }

  def serviceUser[F[_]: Async: Trace: Network: Logger](c: Config): F[Option[User]] =
    c.ssoClient.use: sso =>
      sso.get(Authorization(Credentials.Token(CIString("Bearer"), c.serviceJwt)))

  def topics[F[_]: Concurrent: Logger: Trace](pool: Resource[F, Session[F]]):
   Resource[F, (Topic[F, ObservationTopic.Element], Topic[F, CalibTimeTopic.Element])] =
    for {
      sup <- Supervisor[F]
      ses <- pool
      ctt <- Resource.eval(CalibTimeTopic(ses, 1024, sup))
      top <- Resource.eval(ObservationTopic(ses, 1024, sup))
    } yield (top, ctt)

  def runCalibrationsDaemon[F[_]: Async: Logger](
    emailConfig: Config.Email,
    httpClient: Client[F],
    obsTopic: Topic[F, ObservationTopic.Element],
    calibTopic: Topic[F, CalibTimeTopic.Element],
    services: Resource[F, Services[F]]
  ): Resource[F, Unit] =
    for {
      _  <- Resource.eval(Logger[F].info("Start listening for program changes"))
      _  <- Resource.eval(obsTopic.subscribe(100).evalMap { elem =>
              services.useTransactionally{
                requireServiceAccess:
                  for {
                    t <- Sync[F].delay(LocalDate.now(ZoneOffset.UTC))
                    _ <- calibrationsService(emailConfig, httpClient)
                          .recalculateCalibrations(
                            elem.programId,
                            LocalDateTime.of(t, LocalTime.MIDNIGHT).toInstant(ZoneOffset.UTC)
                          )
                  } yield Result.unit
              }
            }.compile.drain.start.void)
      _  <- Resource.eval(Logger[F].info("Start listening for calibration time changes"))
      _  <- Resource.eval(calibTopic.subscribe(100).evalMap { elem =>
              services.useTransactionally {
                requireServiceAccess:
                  calibrationsService(emailConfig, httpClient).recalculateCalibrationTarget(elem.programId, elem.observationId)
                    .map(Result.success)
              }
            }.compile.drain.start.void)
    } yield ()

  def services[F[_]: Concurrent: Parallel: UUIDGen: Trace: Logger](
    user: Option[User],
    enums: Enums
  )(pool: Session[F]): F[Services[F]] =
    user match {
      case Some(u) if u.role.access === Access.Service =>
        Services.forUser(u, enums, None)(pool).pure[F].flatTap { _ =>
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
  def server[F[_]: Async: Parallel: Logger: Trace: Console: Network: SecureRandom]: Resource[F, ExitCode] =
    for {
      c           <- Resource.eval(Config.fromCiris.load[F])
      _           <- Resource.eval(banner[F](c))
      ep          <- entryPointResource(c)
      pool        <- databasePoolResource[F](c.database)
      enums       <- Resource.eval(pool.use(Enums.load))
      (obsT, ctT) <- topics(pool)
      user        <- Resource.eval(serviceUser[F](c))
      httpClient  <- c.httpClientResource
      _           <- runCalibrationsDaemon(c.email, httpClient, obsT, ctT, pool.evalMap(services(user, enums)))
    } yield ExitCode.Success

  /** Our logical entry point. */
  def runF[F[_]:   Async: Parallel: Logger: Trace: Network: Console: SecureRandom]: F[ExitCode] =
    server.use(_ => Concurrent[F].never[ExitCode])

}

