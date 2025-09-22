// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.cache

import boopickle.DefaultBasic.*
import cats.Hash
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

import java.nio.ByteBuffer
import java.nio.charset.Charset
import scala.concurrent.duration.FiniteDuration

/**
 * Keys are formed with an optional prefix and a hash of the request.
 *
 * Values are stored in binary via boopickle.
 */
trait BinaryEffectfulCache[F[_]: MonadCancelThrow: Logger]
    extends EffectfulCache[F, Array[Byte], Array[Byte]]:
  protected val KeyCharset = Charset.forName("UTF8")

  private def keyToBinary[K1: Hash](key: K1, keyPrefix: String): Array[Byte] =
    val hash: Int      = Hash[K1].hash(key)
    val keyStr: String = s"$keyPrefix:$hash"
    keyStr.getBytes(KeyCharset)

  private def valueToBinary[V1: Pickler](value: V1): Array[Byte] =
    Pickle.intoBytes(value).compact().array()

  private def valueFromBinary[V1: Pickler](bytes: Array[Byte]): F[V1] =
    Either.catchNonFatal(Unpickle[V1].fromBytes(ByteBuffer.wrap(bytes))).liftTo[F]

  def readBinary[K1: Hash, V1: Pickler](key: K1, keyPrefix: String = ""): F[Option[V1]] =
    read(keyToBinary(key, keyPrefix)).flatMap(_.traverse(valueFromBinary))

  def writeBinary[K1: Hash, V1: Pickler](
    key:       K1,
    value:     V1,
    ttl:       Option[FiniteDuration],
    keyPrefix: String = ""
  ): F[Unit] =
    write(keyToBinary(key, keyPrefix), valueToBinary(value), ttl)

  def deleteBinary[K1: Hash](key: K1, keyPrefix: String = ""): F[Unit] =
    delete(keyToBinary(key, keyPrefix))

  def getOrInvokeBinary[K1: Hash, V1: Pickler](
    key:       K1,
    effect:    F[V1],
    ttl:       Option[FiniteDuration],
    keyPrefix: String = ""
  ): F[V1] =
    val bk = keyToBinary(key, keyPrefix)

    def recoverFromCorruption: F[V1] =
      for
        _     <- delete(bk)
        value <- effect
        _     <- write(bk, valueToBinary(value), ttl).attempt.void
      yield value

    def safeValueFromBinary(bytes: Array[Byte]): F[V1] =
      valueFromBinary(bytes).handleErrorWith: e =>
        Logger[F].warn(e)(s"Binary decoding failed recalculate") *>
          recoverFromCorruption

    val writeValue: F[V1] =
      effect.flatTap(r => write(bk, valueToBinary(r), ttl).attempt)

    // Use our own getOrInvoke logic to distinguish between cached and fresh data
    keySemaphore(bk).permit.use: _ =>
      for
        _          <- Logger[F].debug(s"Reading from cache with key [$bk]")
        cacheValue <-
          read(bk)
            .handleErrorWith: e =>
              Logger[F].error(e)(s"Error reading from cache with key [$bk]").as(none)
        r          <- cacheValue.fold(writeValue)(safeValueFromBinary)
      yield r
