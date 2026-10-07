// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.effect.Sync
import cats.effect.unsafe.IORuntime
import io.circe.Encoder
import io.circe.Json

/** A coarse snapshot of the cats-effect compute pool and the JVM heap. */
final case class RuntimeStats(
  workerThreads:        Int,
  activeThreads:        Int,
  blockedWorkerThreads: Int,
  suspendedFibers:      Long,
  heapUsedPercent:      Int
)

object RuntimeStats:

  given Encoder[RuntimeStats] = Encoder.instance: s =>
    Json.obj(
      "workerThreads"        -> Json.fromInt(s.workerThreads),
      "activeThreads"        -> Json.fromInt(s.activeThreads),
      "blockedWorkerThreads" -> Json.fromInt(s.blockedWorkerThreads),
      "suspendedFibers"      -> Json.fromLong(s.suspendedFibers),
      "heapUsedPercent"      -> Json.fromInt(s.heapUsedPercent)
    )

  /** Reads the live numbers. Pool counters are zero when `runtime` is not a work-stealing pool. */
  def current[F[_]: Sync](runtime: IORuntime): F[RuntimeStats] =
    Sync[F].delay:
      val jvm  = Runtime.getRuntime
      val used = jvm.totalMemory - jvm.freeMemory
      val heap = ((used.toDouble / jvm.maxMemory.toDouble) * 100).round.toInt
      runtime.metrics.workStealingThreadPool.fold(
        RuntimeStats(0, 0, 0, 0L, heap)
      ): pool =>
        RuntimeStats(
          workerThreads        = pool.workerThreadCount(),
          activeThreads        = pool.activeThreadCount(),
          blockedWorkerThreads = pool.blockedWorkerThreadCount(),
          suspendedFibers      = pool.suspendedFiberCount(),
          heapUsedPercent      = heap
        )
