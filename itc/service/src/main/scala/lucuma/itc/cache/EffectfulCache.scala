// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.cache

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import io.chrisdavenport.keysemaphore.KeySemaphore
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration

/**
 * Basic cache implementation that will invoke an effect to resolve an absent entry.
 *
 * Provides contention so that multiple concurrent requests for the same key are handled in a single
 * effect.
 */
trait EffectfulCache[F[_]: MonadCancelThrow: Trace: Logger, K, V]:
  protected val keySemaphore: KeySemaphore[F, K]

  protected val L: Logger[F] = Logger[F]

  protected def read(key: K): F[Option[V]]

  protected def write(key: K, value: V, ttl: Option[FiniteDuration]): F[Unit]

  protected def delete(key: K): F[Unit]

  def flush: F[Unit]

  def getOrInvoke(key: K, effect: F[V], ttl: Option[FiniteDuration]): F[V] =
    val whenFound: F[Unit] =
      L.debug(s"Key [$key] found on cache")

    val whenMissing: F[V] =
      for
        _ <- L.debug(s"Key [$key] not found on cache")
        r <- Trace[F].span("cache-request-call")(effect)
        _ <-
          write(key, r, ttl)
            .handleErrorWith(L.error(_)(s"Error writing to cache with key [$key]"))
      yield r

    // Make sure we don't invoke effect multiple times while it's already executing.
    keySemaphore(key).permit.use: _ =>
      Trace[F].span("cache-read"):
        for
          _          <- Trace[F].put("cache.key" -> key.toString)
          _          <- L.debug(s"Reading from cache with key [$key]")
          cacheValue <-
            read(key)
              .handleErrorWith: e =>
                L.error(e)(s"Error reading from cache with key [$key]").as(none)
          r          <- cacheValue.fold(whenMissing)(whenFound.as(_))
        yield r
