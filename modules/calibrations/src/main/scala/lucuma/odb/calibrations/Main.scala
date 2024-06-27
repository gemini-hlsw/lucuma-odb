// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.calibrations

import cats.*
import cats.data.Kleisli
import cats.effect.*
import cats.effect.syntax.all.*
import cats.effect.std.Console
import cats.implicits.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import fs2.io.net.Network
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.util.CommitHash
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import org.http4s.*
import org.http4s.server.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import skunk.{Command as _, *}

import scala.concurrent.duration.*
import lucuma.odb.Config
import lucuma.odb.graphql.topic.ProgramTopic
import cats.effect.std.Supervisor
import fs2.concurrent.Topic
import lucuma.odb.graphql.topic.ObservationTopic
import lucuma.core.model.User
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import lucuma.odb.service.Services
import lucuma.core.model.Role
import lucuma.core.model.Access
import cats.effect.std.UUIDGen

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
            |Port        : ${config.port}
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
      max      = config.maxConnections,
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

  def singleSession[F[_]: Async: Trace: Console: Network](
    config:   Config.Database,
    database: Option[String] = None
  ): Resource[F, Session[F]] = {

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

  implicit def kleisliLogger[F[_]: Logger, A]: Logger[Kleisli[F, A, *]] =
    Logger[F].mapK(Kleisli.liftK)

  def serviceUser[F[_]: Async: Trace: Network: Logger](c: Config): F[Option[User]] =
    c.ssoClient.use: sso =>
      sso.get(Authorization(Credentials.Token(CIString("Bearer"), c.serviceJwt)))
  // This derivation is required to avoid a deprecation warning in the call to wsLiftR below.
  // given [F[_]: Async, A]: Network[Kleisli[F, A, _]] = Network.forAsync

  def topics[F[_]: Concurrent: Logger](pool: Resource[F, Session[F]]): Resource[F, Topic[F, ObservationTopic.Element]] =
    for {
      sup <- Supervisor[F]
      ses <- pool
      top <- Resource.eval(ObservationTopic(ses, 1024, sup))
    } yield top

  def calibrationService[F[_]: Concurrent: Logger: Services](obsTopic: Topic[F, ObservationTopic.Element]): Resource[F, Unit] =
    for {
      _ <- Resource.eval(Logger[F].info("--------- Starting calibration service"))
      cs <- Resource.pure(CalibrationsService.instantiate[F])
      _  <- Resource.eval(obsTopic.subscribe(100).evalMap { elem =>
              Logger[F].info(s"STATE $elem") *> cs.recalculateCalibrations(elem.programId)
            }.compile.drain.start.void)
    } yield ()

  def services[F[_]: Concurrent: UUIDGen: Trace: Logger](user: Option[User], enums: Enums)(pool: Session[F]): F[Services[F]] =
    user match {
      case Some(u) if u.role.access === Access.Service =>
        Services.forUser(u, enums)(pool).pure[F]
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
  def server[F[_]: Async: Logger: Trace: Console: Network]: Resource[F, ExitCode] =
    for {
      c                 <- Resource.eval(Config.fromCiris.load[F])
      _                 <- Resource.eval(banner[F](c))
      s                 <- singleSession(c.database)
      ep                <- entryPointResource(c)
      pool              <- databasePoolResource[F](c.database)
      enums             <- Resource.eval(pool.use(Enums.load))
      obsT              <- topics(pool)
      user              <- Resource.eval(serviceUser[F](c))
      given Services[F] <- pool.evalMap(services(user, enums)(_))
      _                 <- calibrationService(obsT)
    } yield ExitCode.Success

  /** Our logical entry point. */
  def runF[F[_]:   Async: Logger: Trace: Network: Console]: F[ExitCode] =
    server.use(_ => Concurrent[F].never[ExitCode])

}

