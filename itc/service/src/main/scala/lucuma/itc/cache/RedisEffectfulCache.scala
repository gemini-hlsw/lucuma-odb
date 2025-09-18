// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.cache

import cats.effect.Async
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.Flush
import dev.profunktor.redis4cats.algebra.StringCommands
import io.chrisdavenport.keysemaphore.KeySemaphore
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

trait RedisEffectfulCache[F[_]: Async](
  redis:                      StringCommands[F, Array[Byte], Array[Byte]] & Flush[F, Array[Byte]],
  protected val keySemaphore: KeySemaphore[F, Array[Byte]]
) extends BinaryEffectfulCache[F]:

  override protected def read(key: Array[Byte]): F[Option[Array[Byte]]] =
    redis.get(key)

  override protected def write(
    key:   Array[Byte],
    value: Array[Byte],
    ttl:   Option[FiniteDuration]
  ): F[Unit] =
    ttl.fold(redis.set(key, value))(redis.setEx(key, value, _))

  override protected def delete(key: Array[Byte]): F[Unit] =
    redis.unsafe(_.del(key)).void

  override def flush: F[Unit] = redis.flushAll

object RedisEffectfulCache:
  def apply[F[_]: Async: Trace: Logger](
    redis: StringCommands[F, Array[Byte], Array[Byte]] & Flush[F, Array[Byte]]
  ): F[RedisEffectfulCache[F]] =
    KeySemaphore
      .of[F, Array[Byte]](_ => 1L) // 1 permit per key
      .map: keySemaphore =>
        new RedisEffectfulCache(redis, keySemaphore) {}
