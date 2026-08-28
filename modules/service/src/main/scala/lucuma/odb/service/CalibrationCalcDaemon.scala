// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.Resource
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Topic
import lucuma.core.util.CalculationState
import lucuma.odb.data.PendingRecalc
import lucuma.odb.graphql.topic.CalibrationCalcTopic
import lucuma.odb.service.Services.Syntax.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.trace.Tracer

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import scala.concurrent.duration.*

/**
 * Daemon that drains the durable calibration-recalculation queue
 * (`t_calibration_calc`), modeled on `TelluricTargetsDaemon`: a startup reset +
 * drain (the reconciliation of rows enqueued while the daemon was down), a
 * NOTIFY event stream, and a periodic poll.
 *
 * Batch paths group claimed rows by program so the per-program GMOS strategy
 * runs once per program.
 */
object CalibrationCalcDaemon:

  // Midnight UTC of the current date.
  private def referenceInstant[F[_]: Async]: F[Instant] =
    Async[F].realTimeInstant.map: now =>
      val d = LocalDate.ofInstant(now, ZoneOffset.UTC)
      LocalDateTime.of(d, LocalTime.MIDNIGHT).toInstant(ZoneOffset.UTC)

  private def groupByProgram(
    pendings: List[PendingRecalc]
  ): List[NonEmptyList[PendingRecalc]] =
    pendings.groupBy(_.programId).values.toList.flatMap(NonEmptyList.fromList)

  // Recalculate one program's worth of claimed rows, then mark each row
  // ready, or retry on failure.
  private def process[F[_]: {Async, Logger, Tracer as T}](
    services: Resource[F, Services[F]]
  )(pendings: NonEmptyList[PendingRecalc]): F[Unit] =
    val pid  = pendings.head.programId
    val oids = pendings.map(_.observationId)
    referenceInstant.flatMap: when =>
      T.span("calibration-calc.recalculate").surround:
        services.useTransactionally:
          Services.asSuperUser:
            calibrationsService.recalculateCalibrations(pid, when, oids) *>
              pendings.traverse_(calibrationCalcService.markReady)
      .handleErrorWith: e =>
        val msg = Option(e.getMessage).getOrElse(e.toString)
        error"Calibration recalculation failed for program $pid, observations ${oids.toList}: $msg" *>
          pendings.traverse_ { p =>
            services.useTransactionally:
              Services.asSuperUser:
                calibrationCalcService.markRetry(p.observationId, msg)
          }

  /**
   * Startup reconciliation: reset `calculating` leftovers, then drain the
   * queue in batches until empty. Processing is sequential to avoid
   * same-program races.
   */
  def startupDrain[F[_]: {Async, Logger, Tracer as T}](
    batchSize: Int,
    services:  Resource[F, Services[F]]
  ): F[Unit] =
    // The claim must commit before `process` runs: `process` opens its own
    // transaction, and its queue updates would deadlock on the row locks.
    def processBatch: F[Int] =
      T.rootSpan("calibration-calc.startup.batch").surround:
        services.useTransactionally:
          Services.asSuperUser:
            calibrationCalcService.load(batchSize)
        .flatMap: batch =>
          groupByProgram(batch).traverse_(process(services)).as(batch.length)

    def drain(processed: Int): F[Unit] =
      processBatch.flatMap: count =>
        if count > 0 then
          info"Processed a startup batch of $count recalculations, continuing..." *>
          drain(processed + count)
        else
          info"Startup drain complete, $processed total recalculations".void

    for
      _ <- info"Resetting 'calculating' entries to 'pending'"
      _ <- T.rootSpan("calibration-calc.startup.reset").surround:
             services.useTransactionally:
               Services.asSuperUser:
                 calibrationCalcService.reset
      _ <- info"Draining pending calibration recalculations on startup..."
      _ <- drain(0)
    yield ()

  /** Runs the startup reconciliation, then the event/poll streams in the background. */
  def run[F[_]: {Async, LoggerFactory as LF, Tracer as T}](
    connectionsLimit: Int,
    pollPeriod:       FiniteDuration,
    batchSize:        Int,
    topic:            Topic[F, CalibrationCalcTopic.Element],
    services:         Resource[F, Services[F]]
  ): Resource[F, Unit] =
    given Logger[F] = LF.getLoggerFromName("calibration-calc")

    val WaitToRestart = 5.seconds

    // Live events: on a transition to 'pending', claim the single obs.
    val eventStream: Stream[F, NonEmptyList[PendingRecalc]] =
      topic.subscribe(1024)
        .evalMapFilter: e =>
          Option.when(
            e.oldState.forall(_ =!= CalculationState.Pending) &&
            e.newState.exists(_ === CalculationState.Pending)
          )(e.observationId)
            .flatTraverse: oid =>
              T.rootSpan("calibration-calc.event").surround:
                services.useTransactionally:
                  Services.asSuperUser:
                    calibrationCalcService.loadObs(oid).map(_.map(NonEmptyList.one))

    val pollStream: Stream[F, NonEmptyList[PendingRecalc]] =
      Stream
        .awakeEvery(pollPeriod)
        .evalMap: _ =>
          // These polls are noisy and not very useful to trace.
          T.noopScope:
            services.useTransactionally:
              Services.asSuperUser:
                calibrationCalcService.load(connectionsLimit)
        .map(groupByProgram)
        .flatMap(Stream.emits)

    // A failure that escapes `process` (e.g. markRetry losing its connection)
    // can strand claimed rows in `calculating`, which `load` excludes. Re-pend
    // them on every (re)start of the stream, not just at daemon startup.
    val resetStranded: F[Unit] =
      T.rootSpan("calibration-calc.reset").surround:
        services.useTransactionally:
          Services.asSuperUser:
            calibrationCalcService.reset

    // Sequential on purpose: parallel workers could recalculate the same
    // program concurrently and race on its shared calibration set. Recalc is
    // infrequent, so there is no throughput to protect.
    val mainStream: Stream[F, Unit] =
      (Stream.exec(resetStranded) ++
        eventStream
          .merge(pollStream)
          .evalMap(process(services)))
        .attempts(Stream.constant(WaitToRestart))
        .evalTap:
          case Left(e)  => error"Calibration calc daemon error: ${e.getMessage}, restarting in $WaitToRestart..."
          case Right(_) => ().pure[F]
        .void

    Resource.eval:
      for
        _ <- info"Calibration Calc Daemon starting"
        _ <- startupDrain(batchSize, services)
        _ <- info"Starting calibration calc event/poll streams"
        _ <- mainStream.compile.drain.start.void
      yield ()
