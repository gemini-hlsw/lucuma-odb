// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.effect.*
import cats.effect.kernel.Ref
import cats.effect.std.Supervisor
import cats.implicits.*
import cats.kernel.Order
import org.typelevel.log4cats.Logger

import java.time.Instant
import scala.collection.immutable.TreeMap
import scala.concurrent.duration.FiniteDuration

trait Cache[F[_], K, V] {
  def get(k: K): F[Option[V]]
  def put(k: K, v: V): F[Unit]
}

object Cache {

  /** A cache that expires entries after `ttl` and reaps them within `ttl * 2`. */
  def timed[F[_]: Temporal: Logger: Ref.Make, K: Order, V](ttl: FiniteDuration): Resource[F, Cache[F, K, V]] =
    for {
      sv  <- Supervisor[F]
      ref <- Resource.eval(Ref[F].of(TreeMap.empty[K, (V, Instant)]))
      _   <- Resource.eval(sv.supervise(reap(ref, ttl)))
    } yield new Cache[F, K, V] {

      def get(k: K): F[Option[V]] =
        Temporal[F].realTimeInstant.flatMap { now =>
          ref.get.map { data =>
            data.get(k).flatMap { case (v, exp) =>
              if (exp.isAfter(now)) Some(v) else None
            }
          }
        }

      def put(k: K, v: V): F[Unit] =
        Temporal[F].realTimeInstant.flatMap { now =>
          ref.update { data =>
            data + (k -> (v, now.plusMillis(ttl.toMillis)))
          }
        }

    }

  private def reap[F[_]: Temporal: Logger, K, V](ref: Ref[F, TreeMap[K, (V, Instant)]], period: FiniteDuration): F[Unit] =
    Temporal[F].sleep(period) >>
    Temporal[F].realTimeInstant.flatMap { now =>
      ref.modify { data =>
        val prevSize = data.size
        val newData  = data.filter { case (_, (_, exp)) => exp.isAfter(now) }
        (newData, prevSize - newData.size)
      } .flatMap { removed =>
        Logger[F].info(s"Cache: removed $removed expired cache entries.").whenA(removed > 0)
      }
    } >> reap(ref, period)

}