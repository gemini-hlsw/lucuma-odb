// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.tests

import cats.Applicative
import cats.ApplicativeThrow
import cats.syntax.applicative.*
import cats.syntax.option.*
import dev.profunktor.redis4cats.algebra.Flush
import dev.profunktor.redis4cats.algebra.StringCommands
import dev.profunktor.redis4cats.effects.FlushMode
import dev.profunktor.redis4cats.effects.GetExArg
import dev.profunktor.redis4cats.effects.SetArgs
import io.lettuce.core.RedisFuture
import io.lettuce.core.cluster.api.async.RedisClusterAsyncCommands

import scala.concurrent.duration.*

class NoOpRedis[F[_]: ApplicativeThrow, K, V] extends StringCommands[F, K, V] with Flush[F, K] {
  override def getEx(key: K, getExArg: GetExArg): F[Option[V]] =
    none.pure[F]

  override def unsafe[A](f: RedisClusterAsyncCommands[K, V] => RedisFuture[A]): F[A] =
    ApplicativeThrow[F].raiseError(new RuntimeException("unsuppported"))

  override def getSet(key: K, value: V): F[Option[V]] = none.pure[F]

  override def get(key: K): F[Option[V]] = none.pure[F]

  override def mSetNx(keyValues: Map[K, V]): F[Boolean] = true.pure[F]

  override def getRange(key: K, start: Long, end: Long): F[Option[V]] = none.pure[F]

  override def mGet(keys: Set[K]): F[Map[K, V]] = Map.empty.pure[F]

  override def unsafeSync[A](f: RedisClusterAsyncCommands[K, V] => A): F[A] =
    ApplicativeThrow[F].raiseError(new RuntimeException("unsuppported"))

  override def incr(key: K): F[Long] = 1L.pure[F]

  override def decrBy(key: K, amount: Long): F[Long] = (amount - 1).pure[F]

  override def strLen(key: K): F[Long] = 0L.pure[F]

  override def mSet(keyValues: Map[K, V]): F[Unit] = Applicative[F].unit

  override def decr(key: K): F[Long] = 1L.pure[F]

  override def incrByFloat(key: K, amount: Double): F[Double] = (amount + 1).pure[F]

  override def set(key: K, value: V, setArgs: SetArgs): F[Boolean] = true.pure[F]

  override def set(key: K, value: V): F[Unit] = Applicative[F].unit

  override def setNx(key: K, value: V): F[Boolean] = true.pure[F]

  override def setRange(key: K, value: V, offset: Long): F[Unit] = Applicative[F].unit

  override def setEx(key: K, value: V, expiresIn: FiniteDuration): F[Unit] = Applicative[F].unit

  override def incrBy(key: K, amount: Long): F[Long] = (amount + 1).pure[F]

  override def append(key: K, value: V): F[Unit] = Applicative[F].unit

  override def flushAll: F[Unit] = Applicative[F].unit

  override def flushAll(mode: FlushMode): F[Unit] = Applicative[F].unit

  override def flushDb: F[Unit] = Applicative[F].unit

  override def flushDb(mode: FlushMode): F[Unit] = Applicative[F].unit

  override def keys(key: K): F[List[K]] = List.empty.pure[F]
}
