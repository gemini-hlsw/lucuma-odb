// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Encoder
import lucuma.core.util.Gid
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

/**
 * Typeclass for producing an `Array[Byte]` that represents an object.  This is
 * precursor to producing, say, an MD5 hash.
 */
trait HashBytes[A] {

  def hashBytes(a: A): Array[Byte]

  def md5(a: A): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(hashBytes(a))

}

object HashBytes {

  def apply[A](using ev: HashBytes[A]): ev.type = ev

  def forJsonEncoder[A: Encoder]: HashBytes[A] =
    new HashBytes[A] {
      def hashBytes(a: A): Array[Byte] =
        Encoder[A].apply(a).spaces2.getBytes(UTF_8)
    }

  private def toByteArray(b: BigInt, pad: Int): Array[Byte] =
    b.toByteArray.reverse.padTo(pad, 0.toByte)

  given HashBytes[Long] with {
    def hashBytes(a: Long): Array[Byte] =
      toByteArray(BigInt(a), 8)
  }

  given HashBytes[PosLong] with {
    def hashBytes(a: PosLong): Array[Byte] =
      HashBytes[Long].hashBytes(a.value)
  }

  given HashBytes[Int] with {
    def hashBytes(a: Int): Array[Byte] =
      toByteArray(BigInt(a), 4)
  }

  given HashBytes[PosInt] with {
    def hashBytes(a: PosInt): Array[Byte] =
      HashBytes[Int].hashBytes(a.value)
  }

  given HashBytes[Char] with {
    def hashBytes(a: Char): Array[Byte] =
      toByteArray(BigInt(a), 2)
  }

  given given_HashBytes_Gid[A: Gid]: HashBytes[A] with {
    def hashBytes(a: A): Array[Byte] =
      Array.concat(
        HashBytes[Char].hashBytes(Gid[A].tag.value),
        HashBytes[PosLong].hashBytes(Gid[A].isoPosLong.get(a))
      )
  }

  given HashBytes[Timestamp] with {
    def hashBytes(a: Timestamp): Array[Byte] =
      HashBytes[Long].hashBytes(a.toEpochMilli)
  }

  given HashBytes[TimeSpan] with {
    def hashBytes(a: TimeSpan): Array[Byte] =
      HashBytes[Long].hashBytes(a.toMicroseconds)
  }

  given [A](using HashBytes[A]): HashBytes[Option[A]] with {
    def hashBytes(opt: Option[A]): Array[Byte] =
      opt.fold(Array.emptyByteArray)(HashBytes[A].hashBytes)
  }
}
