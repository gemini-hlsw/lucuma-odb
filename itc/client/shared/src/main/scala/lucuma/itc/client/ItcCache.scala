// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.effect.Ref
import cats.effect.Sync
import cats.syntax.all.*

trait ItcCache[F[_], K, V] {

  def get(key: K): F[Option[V]]

  def put(key: K)(value: V): F[Unit]

  def getOrCalc(key: K)(value: => V): F[V]

  def getOrCalcF(key: K)(value: => F[V]): F[V]

  def remove(key: K): F[Unit]

  def flush: F[Unit]

}

object ItcCache {

  def simple[F[_]: Sync, K, V]: F[ItcCache[F, K, V]] =
    Ref.of[F, Map[K, V]](Map.empty[K, V]).map { ref =>
      new ItcCache[F, K, V] {

        override def get(key: K): F[Option[V]] =
          ref.get.map(_.get(key))

        override def put(key: K)(value: V): F[Unit] =
          ref.update(_.updated(key, value))

        override def getOrCalc(key: K)(value: => V): F[V] =
          ref.modify { m =>
            m.get(key).fold((m.updated(key, value), m(key)))((m, _))
          }

        override def getOrCalcF(key: K)(value: => F[V]): F[V] =
          for {
            ov <- get(key)
            v  <- ov.fold(value.flatTap(put(key)))(_.pure)
          } yield v

        override def remove(key: K): F[Unit] =
          ref.update(_.removed(key))

        override def flush: F[Unit] =
          ref.set(Map.empty[K, V])

      }
    }

}
