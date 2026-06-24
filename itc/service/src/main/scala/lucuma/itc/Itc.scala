// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.effect.Clock
import cats.effect.Sync
import cats.effect.std.Semaphore
import cats.effect.syntax.all.*
import cats.syntax.all.*
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetTimeAndGraphs
import org.typelevel.log4cats.Logger

import java.util.concurrent.atomic.AtomicInteger

trait Itc[F[_]]:

  /**
   * Retrieve the integration time for the given mode and exposureTimeMode.
   */
  def calculate(
    target:           TargetData,
    observingMode:    ObservingMode,
    constraints:      ItcObservingConditions,
    exposureTimeMode: ExposureTimeMode
  ): F[TargetIntegrationTime]

  def calculateTimeAndGraphs(
    target:           TargetData,
    observingMode:    ObservingMode,
    constraints:      ItcObservingConditions,
    exposureTimeMode: ExposureTimeMode
  ): F[TargetTimeAndGraphs]

object Itc:
  def apply[F[_]](using ev: Itc[F]): ev.type = ev

  /**
   * Wrap an `Itc` so that at most `maxConcurrent` heavy (legacy OCS) calculations run concurrently.
   * Only cache misses reach this (cache hits never invoke the calculation), so this caps the
   * expensive work without throttling cached responses.
   *
   * It also logs, for each calculation: how long it waited for a permit (queue backlog), how long
   * the calculation took, and the live concurrency level. This is meant to isolate whether requests
   * are backing up (contention) vs. individual calculations being slow.
   */
  def limitConcurrency[F[_]: {Sync as F, Logger as L}](
    maxConcurrent: Int,
    sem:           Semaphore[F]
  )(underlying: Itc[F]): Itc[F] =
    val running = new AtomicInteger(0)

    def limited[A](label: String)(fa: F[A]): F[A] =
      Clock[F].monotonic.flatMap: queuedAt =>
        sem.permit.use: _ =>
          for
            acquiredAt <- Clock[F].monotonic
            r          <- F.delay(running.incrementAndGet())
            _          <-
              L.info(
                s"[itc-compute] $label start waitMs=${(acquiredAt - queuedAt).toMillis} running=$r/$maxConcurrent"
              )
            a          <- fa.guarantee(F.delay(running.decrementAndGet()).void)
            doneAt     <- Clock[F].monotonic
            _          <-
              L.info(
                s"[itc-compute] $label done computeMs=${(doneAt - acquiredAt).toMillis} running=${running.get}/$maxConcurrent"
              )
          yield a

    new Itc[F]:
      def calculate(
        target:           TargetData,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureTimeMode: ExposureTimeMode
      ): F[TargetIntegrationTime] =
        limited(s"calc ${observingMode.instrument}"):
          underlying.calculate(target, observingMode, constraints, exposureTimeMode)

      def calculateTimeAndGraphs(
        target:           TargetData,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureTimeMode: ExposureTimeMode
      ): F[TargetTimeAndGraphs] =
        limited(s"graphs ${observingMode.instrument}"):
          underlying.calculateTimeAndGraphs(target, observingMode, constraints, exposureTimeMode)
