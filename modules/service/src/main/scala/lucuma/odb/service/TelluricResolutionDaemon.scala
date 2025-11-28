// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Parallel
import cats.effect.Async
import cats.effect.Resource
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Topic
import lucuma.core.util.CalculationState
import lucuma.odb.data.EditType
import lucuma.odb.data.TelluricResolution
import lucuma.odb.graphql.topic.ObscalcTopic
import lucuma.odb.graphql.topic.TelluricTargetTopic
import lucuma.odb.service.Services.Syntax.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*

import scala.concurrent.duration.*

object TelluricResolutionDaemon:

  /**
   * Run the telluric resolution daemon.
   *
   * @param connectionsLimit max concurrent resolutions
   * @param pollPeriod how often to poll for pending/retry entries
   * @param batchSize process this many on each startup batch iteration
   * @param topic topic for resolution events
   * @param obscalcTopic topic for obscalc events (science observation changes)
   * @param services service layer
   */
  def run[F[_]: Async: Parallel: LoggerFactory](
    connectionsLimit: Int,
    pollPeriod:       FiniteDuration,
    batchSize:        Int,
    topic:            Topic[F, TelluricTargetTopic.Element],
    obscalcTopic:     Topic[F, ObscalcTopic.Element],
    services:         Resource[F, Services[F]]
  ): Resource[F, Unit] =
    given Logger[F] = LoggerFactory[F].getLoggerFromName("telluric-targets")

    // Stream of pending entries produced by watching for updates.
    // Filter for transitions TO 'pending' state to naturally debounce.
    val eventStream: Stream[F, TelluricResolution.Pending] =
      topic.subscribe(1024).evalMapFilter: e =>
        Option
          .when(
            e.oldState.forall(_ =!= CalculationState.Pending) &&
            e.newState.exists(_ === CalculationState.Pending)
          )(e.observationId)
          .flatTraverse: oid =>
            services.useTransactionally:
              Services.asSuperUser:
                telluricResolutionService.loadObs(oid)

    // Stream of pending entries produced by periodic polling.
    // Picks up 'pending' and 'retry' entries (including those past retry_at time).
    val pollStream: Stream[F, TelluricResolution.Pending] =
      Stream
        .awakeEvery(pollPeriod)
        .evalMap: _ =>
          services.useTransactionally:
            Services.asSuperUser:
              telluricResolutionService.load(connectionsLimit)
        .flatMap(Stream.emits)

    // Stream that rechecks resolved telluric targets when science observations change.
    // Triggered by obscalc events when calculation completes (Ready state).
    val recheckStream: Stream[F, Unit] =
      obscalcTopic.subscribe(100).evalMapFilter: e =>
        // Only recheck when science observation's obscalc becomes Ready
        Option.when(
          e.newState.exists(_ === CalculationState.Ready) &&
          (e.editType === EditType.Created || e.editType === EditType.Updated)
        )(e.observationId).traverse: scienceObsId =>
          // Check if this observation has any associated telluric calibrations
          services.useTransactionally:
            Services.asSuperUser:
              calibrationsService.isCalibration(scienceObsId)
          .flatMap: isCalib =>
            if isCalib then
              ().pure[F] // Skip calibration observations
            else
              info"Science observation $scienceObsId changed, rechecking telluric targets" *>
              services.useNonTransactionally:
                Services.asSuperUser:
                  telluricResolutionService.recheckForScienceObservation(scienceObsId)

    // Combine event and poll streams, process in parallel (outside transactions).
    val resolveAndUpdateStream: Stream[F, Unit] =
      eventStream
        .merge(pollStream)
        .evalTap: pending =>
          debug"Loaded pending resolution ${pending.observationId}"
        .parEvalMapUnordered(connectionsLimit): pending =>
          services.useNonTransactionally:  // CRITICAL: outside transaction
            Services.asSuperUser:
              telluricResolutionService
                .resolveAndUpdate(pending)
                .map((pending, _))
        .evalTap: result =>
          val (pending, metaOpt) = result
          debug"Resolved telluric for ${pending.observationId}: $metaOpt"
        .void

    // Process all pending on startup in batches
    def startupBatch: F[Unit] =
      def processBatch: F[Boolean] =
        services.useTransactionally:
          Services.asSuperUser:
            telluricResolutionService.load(batchSize)
        .flatMap: batch =>
          if batch.isEmpty then
            false.pure[F]
          else
            batch.parTraverse_(pending =>
              services.useNonTransactionally:
                Services.asSuperUser:
                  telluricResolutionService.resolveAndUpdate(pending)
            )
            .as(true)

      def loop(processed: Int): F[Unit] =
        processBatch.flatMap: hasMore =>
          if hasMore then
            info"Processed $batchSize resolutions on startup, continuing..." *>
            loop(processed + batchSize)
          else
            info"Startup batch processing complete, processed $processed total resolutions".void

      info"Starting batch processing of all pending telluric resolutions..." *>
      loop(0)

    for
      _ <- Resource.eval(info"Resetting 'calculating' entries to 'pending'")
      _ <- Resource.eval:
             services.useTransactionally:
               Services.asSuperUser:
                 telluricResolutionService.reset
      _ <- Resource.eval(info"Processing pending resolutions on startup")
      _ <- Resource.eval(startupBatch)
      _ <- Resource.eval(info"Starting telluric resolution event/poll streams")
      _ <- Resource.eval(resolveAndUpdateStream.compile.drain.start.void)
      _ <- Resource.eval(info"Starting telluric recheck stream for obscalc events")
      _ <- Resource.eval(recheckStream.compile.drain.start.void)
    yield ()
