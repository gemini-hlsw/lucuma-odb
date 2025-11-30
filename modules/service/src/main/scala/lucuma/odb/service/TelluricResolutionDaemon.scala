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
   */
  def run[F[_]: Async: Parallel: LoggerFactory](
    connectionsLimit: Int,
    pollPeriod:       FiniteDuration,
    batchSize:        Int,
    topic:            Topic[F, TelluricTargetTopic.Element],
    obscalcTopic:     Topic[F, ObscalcTopic.Element],
    services:         Resource[F, Services[F]]
  ): F[Unit] =
    given Logger[F] = LoggerFactory[F].getLoggerFromName("telluric-targets")

    // Stream of pending requestes
    // Filter for transitions TO 'pending' state
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

    // pending entries to handle 'pending' and 'retry' entries
    val pollStream: Stream[F, TelluricResolution.Pending] =
      Stream
        .awakeEvery(pollPeriod)
        .evalMap: _ =>
          services.useTransactionally:
            Services.asSuperUser:
              telluricResolutionService.load(connectionsLimit)
        .flatMap(Stream.emits)

    // Recheck telluric targets when science observations change.
    // Triggered by obscalc events ready
    val recheckStream: Stream[F, Unit] =
      obscalcTopic.subscribe(100).evalMapFilter: e =>
        Option.when(
          e.newState.exists(_ === CalculationState.Ready) && // Only ready obscalc
          (e.editType === EditType.Created || e.editType === EditType.Updated)
        )(e.observationId).traverse: oid =>
          services.useTransactionally:
            Services.asSuperUser:
              calibrationsService.isCalibration(oid)
          .flatMap: isCalib =>
            (info"Science observation $oid changed, rechecking telluric targets" *>
            services.useNonTransactionally:
              Services.asSuperUser:
                telluricResolutionService.recheckForScienceObservation(oid)).unlessA(isCalib)

    val mainStream: Stream[F, Unit] =
      eventStream
        .merge(pollStream)
        .evalTap: pending =>
          debug"Loaded pending resolution ${pending.observationId}"
        .parEvalMapUnordered(connectionsLimit): pending =>
          services.useNonTransactionally:
            Services.asSuperUser:
              telluricResolutionService
                .resolveAndUpdate(pending)
                .map((pending, _))
        .evalTap: result =>
          val (pending, meta) = result
          debug"Resolved telluric for ${pending.observationId}: $meta"
        .void

    // Initial processing on startup
    def startupBatch: F[Unit] =
      def processBatch: F[Boolean] =
        services.useTransactionally:
          Services.asSuperUser:
            telluricResolutionService.load(batchSize)
        .flatMap: batch =>
          if batch.isEmpty then
            false.pure[F]
          else
            batch.parTraverse_ : pending =>
              services.useNonTransactionally:
                Services.asSuperUser:
                  telluricResolutionService.resolveAndUpdate(pending)
            .as(true)

      def runStart(processed: Int): F[Unit] =
        processBatch.flatMap: hasMore =>
          if hasMore then
            info"Processed $batchSize batch resolutions on startup, continuing..." *>
            runStart(processed + batchSize)
          else
            info"Startup processing complete, $processed total resolutions".void

      info"Starting batch processing of all pending telluric resolutions..." *>
      runStart(0)

    for
      _ <- info"Resetting 'calculating' entries to 'pending'"
      _ <- services.useTransactionally:
             Services.asSuperUser:
               telluricResolutionService.reset
      _ <- info"Processing pending resolutions on startup"
      _ <- startupBatch
      _ <- info"Starting telluric resolution event/poll streams"
      _ <- mainStream.compile.drain.start.void
      _ <- info"Starting telluric recheck stream for obscalc events"
      _ <- recheckStream.compile.drain.start.void
    yield ()
