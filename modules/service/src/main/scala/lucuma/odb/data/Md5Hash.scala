// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.eq.*
import scodec.bits.ByteVector

opaque type Md5Hash = ByteVector

object Md5Hash {

  val Zero: Md5Hash = unsafeFromByteArray(Array.fill(16)(0.toByte))

  def fromByteArray(bytes: Array[Byte]): Option[Md5Hash] =
    Option.when(bytes.size === 16)(ByteVector(bytes))

  def unsafeFromByteArray(bytes: Array[Byte]): Md5Hash =
    fromByteArray(bytes).getOrElse(sys.error(s"Expected 16 byte array but was ${bytes.length} bytes"))

  def fromByteVector(bv: ByteVector): Option[Md5Hash] =
    Option.when(bv.size === 16)(bv)

  extension (h: Md5Hash) {
    def toHex: String =
      h.toHex

    def toByteArray: Array[Byte] =
      h.toArray

    def toByteVector: ByteVector =
      h
  }

  given Eq[Md5Hash] =
    Eq.instance[Md5Hash]((a, b) => a.toByteVector === b.toByteVector)

}