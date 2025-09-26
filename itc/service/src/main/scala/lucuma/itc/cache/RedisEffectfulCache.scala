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

trait RedisEffectfulCache[F[_]: Async: Trace](
  redis:                      StringCommands[F, Array[Byte], Array[Byte]] & Flush[F, Array[Byte]],
  protected val keySemaphore: KeySemaphore[F, Array[Byte]]
) extends BinaryEffectfulCache[F]:

  override protected def read(key: Array[Byte]): F[Option[Array[Byte]]] =
    Trace[F].span("redis_read"):
      Trace[F].put("cache.operation" -> "read", "cache.backend" -> "redis") *>
        redis
          .get(key)
          .flatTap: result =>
            Trace[F].put("cache.status" -> (result.fold("miss")(_ => "hit")))

  override protected def readWithContext(
    key:     Array[Byte],
    context: String = ""
  ): F[Option[Array[Byte]]] =
    Trace[F].span("redis_read_with_context"):
      Trace[F].put("cache.operation"  -> "read",
                   "cache.backend"    -> "redis",
                   "cache.key_prefix" -> context
      ) *>
        redis
          .get(key)
          .flatTap: result =>
            Trace[F].put("cache.status" -> (result.fold("miss")(_ => "hit")))

  override protected def write(
    key:   Array[Byte],
    value: Array[Byte],
    ttl:   Option[FiniteDuration]
  ): F[Unit] =
    Trace[F].span("redis_write"):
      Trace[F].put("cache.operation" -> "write",
                   "cache.backend"   -> "redis",
                   "cache.has_ttl"   -> ttl.isDefined.toString
      ) *>
        ttl.fold(redis.set(key, value))(redis.setEx(key, value, _))

  override protected def delete(key: Array[Byte]): F[Unit] =
    Trace[F].span("redis_delete"):
      Trace[F].put("cache.operation" -> "delete", "cache.backend" -> "redis") *>
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
