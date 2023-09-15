// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Encoder
import lucuma.core.util.Gid

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

trait HashBytes[A] {

  def hashBytes(a: A): Array[Byte]

  def md5(a: A): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(hashBytes(a))

}

object HashBytes {

  def apply[A](using ev: HashBytes[A]): ev.type = ev

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

  given given_HashBytes_Json[A: Encoder]: HashBytes[A] with {
    def hashBytes(a: A): Array[Byte] =
      Encoder[A].apply(a).spaces2.getBytes(UTF_8)
  }
}