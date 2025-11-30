// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import fs2.Stream
import fs2.concurrent.Topic
import fs2.io.net.Network
import grackle.Mapping
import grackle.skunk.SkunkMonitor
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.model.Access
import lucuma.core.model.User
import lucuma.core.util.CalculationState
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.data.Obscalc
import lucuma.odb.graphql.OdbMapping
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.topic.ObscalcTopic
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
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
import skunk.{Command as _, *}

import scala.concurrent.duration.*
import scala.util.NotGiven

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
    given LF: LoggerFactory[IO] = Slf4jFactory.create[IO]
    given Logger[IO] = LF.getLoggerFromName("obscalc-service")

    import natchez.Trace.Implicits.noop

    CalcMain.runF[IO]

object CalcMain extends MainParams:

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](config: Config): F[Unit] =
    val banner =
        s"""|
            |$Header
            |
            |CommitHash.....: ${config.commitHash.format}
            |Max Connections: ${config.database.maxObscalcConnections}
            |Poll Period....: ${config.obscalcPoll}
            |PID............: ${ProcessHandle.current.pid}
            |Tracing........: ${LucumaEntryPoint.tracingBackend(config)}
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


  def serviceUser[F[_]: Async: Trace: Network: Logger](c: Config): F[User] =
    c.ssoClient
     .use: sso =>
       sso.get(Authorization(Credentials.Token(CIString("Bearer"), c.serviceJwt)))
     .flatMap:
       case Some(u) if u.role.access === Access.Service =>
         u.pure[F]
       case Some(u) =>
         Logger[F].error(s"User $u is not allowed to execute this service") *>
           MonadThrow[F].raiseError(new RuntimeException(s"User $u doesn't have permission to execute"))
       case None    =>
         Logger[F].error("Failed to get service user") *>
           MonadThrow[F].raiseError(new RuntimeException("Failed to get service user"))

  def topic[F[_]: Concurrent: Logger: Trace](
    pool: Resource[F, Session[F]]
  ): Resource[F, Topic[F, ObscalcTopic.Element]] =
      for
        s <- Supervisor[F]
        p <- pool
        t <- Resource.eval(ObscalcTopic(p, 65536, s))
      yield t

  def runObscalcDaemon[F[_]: Async: Logger](
    connectionsLimit: Int,
    pollPeriod:       FiniteDuration,
    topic:            Topic[F, ObscalcTopic.Element],
    services:         Resource[F, Services[F]]
  ): Resource[F, F[Outcome[F, Throwable, Unit]]] =

    // Stream of pending calc produced by watching for updates to t_obscalc.
    // We filter out anything but transitions to Pending.  Entries in the Retry
    // state are picked up via polling (see pollStream below).  By responding
    // only to events produced by moving to `Pending` we naturally debounce
    // multiple events that happen while 'Calculating'.  At the end of the
    // calculation we'll move back to 'Pending' if there were additional updates
    // or to 'Ready' otherwise.
    val eventStream: Stream[F, Obscalc.PendingCalc] =
      topic.subscribe(65536).evalMapFilter: e =>
        Option
          .when(
            e.oldState.forall(_ =!= CalculationState.Pending) &&
            e.newState.exists(_ === CalculationState.Pending)
          )(e.observationId)
          .flatTraverse: oid =>
            services.useTransactionally:
              requireServiceAccessOrThrow:
                obscalcService.loadObs(oid)

    // Stream of pending calc produced by periodic polling.  This will pick up
    // up to connectionsLimit entries including those that are in a 'Retry'
    // state.
    val pollStream: Stream[F, Obscalc.PendingCalc] =
      Stream
        .awakeEvery(pollPeriod)
        .evalMap: _ =>
          services.useTransactionally:
            requireServiceAccessOrThrow:
              obscalcService.load(1024)
        .flatMap(Stream.emits)

    // Combine the eventStream and the pollStream (after startup), process each
    // pending calc as it appears doing the calculation and storing the results.
    val calcAndUpdateStream: Stream[F, Unit] =
      eventStream
        .merge(pollStream)
        .evalTap: pc =>
          Logger[F].debug(s"Loaded PendingCalc ${pc.observationId}. Last invalidated at ${pc.lastInvalidation}.")
        .parEvalMapUnordered(connectionsLimit): pc =>
          services.useNonTransactionally:
            requireServiceAccessOrThrow:
              obscalcService
                .calculateAndUpdate(pc)
                .tupleLeft(pc)
        .evalTap: (pc, meta) =>
          Logger[F].debug(s"Stored obscalc result for ${pc.observationId}. Current status: $meta.")
        .void

    for
      _ <- Resource.eval(Logger[F].info("Processing PendingCalc"))
      _ <- Resource.eval:
             services.useTransactionally:
               requireServiceAccessOrThrow:
                 obscalcService.reset
      o <- calcAndUpdateStream.compile.drain.background
    yield o

  def services[F[_]: Temporal: Async: Parallel: UUIDGen: Trace: Logger: LoggerFactory](
    user:        User,
    enums:       Enums,
    mapping:     Session[F] => Mapping[F],
    emailConfig: Config.Email,
    commitHash:  CommitHash,
    calculator:  TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient:  Client[F],
    itcClient:   ItcClient[F],
    gaiaClient:  GaiaClient[F],
    horizonsClient: HorizonsClient[F],
    telClient:   TelluricTargetsClient[F]
  )(session: Session[F]): F[Services[F]] =
    Services.forUser(
      user,
      enums,
      mapping.some,
      emailConfig,
      commitHash,
      calculator,
      httpClient,
      itcClient,
      gaiaClient,
      S3FileService.noop[F],
      horizonsClient,
      telClient
    )(session).pure[F].flatTap: _ =>
      val us = UserService.fromSession(session)
      Services.asSuperUser(us.canonicalizeUser(user))

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down
   * in cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server[F[_]: Async: Parallel: Logger: LoggerFactory: Trace: Console: Network: SecureRandom]: Resource[F, F[Outcome[F, Throwable, Unit]]] =
    for
      c          <- Resource.eval(Config.fromCiris.load[F])
      _          <- Resource.eval(banner[F](c))
      ep         <- LucumaEntryPoint.entryPointResource(ServiceName, c)
      pool       <- databasePoolResource[F](c.database)
      enums      <- Resource.eval(pool.use(Enums.load))
      http       <- c.httpClientResource
      gaiaClient <- c.gaiaClient
      itc        <- c.itcClient
      horizonsClient <- c.horizonsClientResource
      telClient  = TelluricTargetsClient.noop[F]
      ptc        <- Resource.eval(pool.use(TimeEstimateCalculatorImplementation.fromSession(_, enums)))
      t          <- topic(pool)
      user       <- Resource.eval(serviceUser[F](c))
      mapping     = (s: Session[F]) =>
                      OdbMapping.forObscalc(
                        Resource.pure(s),
                        SkunkMonitor.noopMonitor[F],
                        user,
                        c.goaUsers,
                        gaiaClient,
                        itc,
                        c.commitHash,
                        enums,
                        ptc,
                        http,
                        horizonsClient,
                        c.email
                      )
      o          <- runObscalcDaemon(
                      c.database.maxObscalcConnections,
                      c.obscalcPoll,
                      t,
                      pool.evalMap(
                        services(
                          user,
                          enums,
                          mapping,
                          c.email,
                          c.commitHash,
                          ptc,
                          http,
                          itc,
                          gaiaClient,
                          horizonsClient,
                          telClient
                      )))
    yield o

  /** Our logical entry point. */
  def runF[F[_]:   Async: Parallel: Logger: LoggerFactory: Trace: Network: Console: SecureRandom]: F[ExitCode] =
    server.use: o =>
      o.flatMap:
        case Outcome.Succeeded(_) => Logger[F].info("Obscalc completed.")  >> ExitCode.Success.pure[F]
        case Outcome.Errored(e)   => Logger[F].error(e)("Obscalc failed.") >> ExitCode.Error.pure[F]
        case Outcome.Canceled()   => Logger[F].info("Obscalc cancelled.")  >> ExitCode.Success.pure[F]
