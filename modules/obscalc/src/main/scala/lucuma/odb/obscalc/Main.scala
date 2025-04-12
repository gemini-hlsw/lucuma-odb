// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.obscalc

import cats.*
import cats.effect.*
import cats.effect.kernel.syntax.spawn.*
import cats.effect.std.Console
import cats.effect.std.SecureRandom
import cats.effect.std.Supervisor
import cats.effect.std.UUIDGen
import cats.implicits.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import fs2.concurrent.Topic
import fs2.io.net.Network
import lucuma.core.model.Access
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.topic.ObscalcTopic
import lucuma.odb.logic.Generator
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ItcService
import lucuma.odb.service.ObscalcService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.UserService
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import org.http4s.Credentials
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import skunk.{Command as _, *}

import scala.concurrent.duration.*

sealed trait MainParams:
  val ServiceName: String =
    "obscalc-service"

  val Header: String =
    s"""|
 ██████╗ ██████╗ ███████╗ ██████╗ █████╗ ██╗      ██████╗
██╔═══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗██║     ██╔════╝
██║   ██║██████╔╝███████╗██║     ███████║██║     ██║
██║   ██║██╔══██╗╚════██║██║     ██╔══██║██║     ██║
╚██████╔╝██████╔╝███████║╚██████╗██║  ██║███████╗╚██████╗
 ╚═════╝ ╚═════╝ ╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝ ╚═════╝
        |This is the Lucuma observation calculations service.
        |""".stripMargin

object MainParams extends MainParams

object ObscalcMain extends CommandIOApp(
  name   = MainParams.ServiceName,
  header = MainParams.Header
):

  override def main: Opts[IO[ExitCode]] =
    Opts(serve)

  lazy val serve: IO[ExitCode] =
    given Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("obscalc-service")

    import natchez.Trace.Implicits.noop

    CalcMain.runF[IO]

object CalcMain extends MainParams:

  val ParallelTaskLimit = 8

  case class PendingCalc(
    observationId:    Observation.Id,
    lastInvalidation: Timestamp
  )

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] =
    val banner =
        s"""|
            |$Header
            |
            |CommitHash. : ${config.commitHash.format}
            |PID         : ${ProcessHandle.current.pid}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))

  /** A resource that yields a Skunk session pool. */
  def databasePoolResource[F[_]: Temporal: Network: Console](
    config: Config.Database
  ): Resource[F, Resource[F, Session[F]]] =
    import natchez.Trace.Implicits.noop
    Session.pooled(
      host     = config.host,
      port     = config.port,
      user     = config.user,
      password = Some(config.password),
      database = config.database,
      ssl      = SSL.Trusted.withFallback(true),
      max      = config.maxObscalcConnections,
      strategy = Strategy.SearchPath,
      // debug    = true,
    )

  /** A resource that yields a Natchez tracing entry point. */
  def entryPointResource[F[_]: Sync](config: Config): Resource[F, EntryPoint[F]] =
    Honeycomb.entryPoint(ServiceName): cb =>
      Sync[F].delay:
        cb.setWriteKey(config.honeycomb.writeKey)
        cb.setDataset(config.honeycomb.dataset)
        cb.build()

  def serviceUser[F[_]: Async: Trace: Network: Logger](c: Config): F[Option[User]] =
    c.ssoClient.use: sso =>
      sso.get(Authorization(Credentials.Token(CIString("Bearer"), c.serviceJwt)))

  def topic[F[_]: Concurrent: Logger](
    pool: Resource[F, Session[F]]
  ): Resource[F, Topic[F, ObscalcTopic.Element]] =
      for
        s <- Supervisor[F]
        p <- pool
        t <- Resource.eval(ObscalcTopic(p, 1024, s))
      yield t

  def runObscalcDaemon[F[_]: Async: Parallel: Logger](
    i: ItcClient[F],
    g: Services[F] ?=> Generator[F],
    t: Topic[F, ObscalcTopic.Element],
    s: Resource[F, Services[F]]
  ): Resource[F, Unit] =
    def obscalc(using Services[F]): ObscalcService[F] =
      ObscalcService.instantiate(ItcService.instantiate(i), g)

    for
      _ <- Resource.eval(Logger[F].info("Start listening for observation calc changes"))
      _ <- Resource.eval(s.useTransactionally(obscalc.resetCalculating))
      _ <- Resource.eval(
             t.subscribe(100)
              .parEvalMapUnordered(ParallelTaskLimit): elem =>
                s.useTransactionally:
                  obscalc.loadPendingCalc(ParallelTaskLimit).void
              .compile
              .drain
              .start
              .void
           )
    yield ()

  def services[F[_]: Concurrent: Parallel: UUIDGen: Trace: Logger: SecureRandom](
    user: Option[User],
    enums: Enums
  )(pool: Session[F]): F[Services[F]] =
    user match
      case Some(u) if u.role.access === Access.Service =>
        Services.forUser(u, enums, None)(pool).pure[F].flatTap: s =>
          val us = UserService.fromSession(pool)
          Services.asSuperUser(us.canonicalizeUser(u))
      case Some(u) =>
        Logger[F].error(s"User $u is not allowed to execute this service") *>
          MonadThrow[F].raiseError(new RuntimeException(s"User $u doesn't have permission to execute"))
      case None    =>
        Logger[F].error("Failed to get service user") *>
          MonadThrow[F].raiseError(new RuntimeException("Failed to get service user"))

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down
   * in cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server[F[_]: Async: Parallel: Logger: Trace: Console: Network: SecureRandom]: Resource[F, ExitCode] =
    for
      c     <- Resource.eval(Config.fromCiris.load[F])
      _     <- Resource.eval(banner[F](c))
      ep    <- entryPointResource(c)
      pool  <- databasePoolResource[F](c.database)
      enums <- Resource.eval(pool.use(Enums.load))

      itc   <- c.itcClient
      ptc   <- Resource.eval(pool.use(TimeEstimateCalculatorImplementation.fromSession(_, enums)))

      t     <- topic(pool)
      user  <- Resource.eval(serviceUser[F](c))
      _     <- runObscalcDaemon(itc, generator(c.commitHash, itc, ptc), t, pool.evalMap(services(user, enums)))
    yield ExitCode.Success

  /** Our logical entry point. */
  def runF[F[_]:   Async: Parallel: Logger: Trace: Network: Console: SecureRandom]: F[ExitCode] =
    server.use(_ => Concurrent[F].never[ExitCode])