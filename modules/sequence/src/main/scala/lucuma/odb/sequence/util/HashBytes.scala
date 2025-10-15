// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Encoder
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

/**
 * Typeclass for producing an `Array[Byte]` that represents an object.  This is
 * precursor to producing, say, an MD5 hash.
 */
trait HashBytes[A]:

  def hashBytes(a: A): Array[Byte]

  def md5(a: A): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(hashBytes(a))

object HashBytes:

  def apply[A](using ev: HashBytes[A]): ev.type = ev

  import lucuma.odb.sequence.syntax.hash.*
//  extension [A: HashBytes](a: A)
//    def hashBytes: Array[Byte] =
//      HashBytes[A].hashBytes(a)

  def by[A, B: HashBytes](f: A => B): HashBytes[A] =
    new HashBytes[A]:
      def hashBytes(a: A): Array[Byte] =
        f(a).hashBytes

  def by2[A, B: HashBytes, C: HashBytes](f: A => B, g: A => C): HashBytes[A] =
    new HashBytes[A]:
      def hashBytes(a: A): Array[Byte] =
        Array.concat(f(a).hashBytes, g(a).hashBytes)

  def forJsonEncoder[A: Encoder]: HashBytes[A] =
    new HashBytes[A] {
      def hashBytes(a: A): Array[Byte] =
        Encoder[A].apply(a).spaces2.getBytes(UTF_8)
    }

  private def toByteArray(b: BigInt, pad: Int): Array[Byte] =
    b.toByteArray.reverse.padTo(pad, 0.toByte)

  given HashBytes[Long] with
    def hashBytes(a: Long): Array[Byte] =
      toByteArray(BigInt(a), 8)

  given HashBytes[Int] with
    def hashBytes(a: Int): Array[Byte] =
      toByteArray(BigInt(a), 4)

  given HashBytes[PosLong] =
    HashBytes.by(_.value)

  given HashBytes[NonNegInt] =
    HashBytes.by(_.value)

  given HashBytes[PosInt] =
    HashBytes.by(_.value)

  given HashBytes[Char] with
    def hashBytes(a: Char): Array[Byte] =
      toByteArray(BigInt(a), 2)

  given HashBytes[String] = _.getBytes

  given HashBytes[Boolean] =
    HashBytes.by(_.hashCode)

  given given_HashBytes_Gid[A: Gid]: HashBytes[A] with
    def hashBytes(a: A): Array[Byte] =
      Array.concat(
        HashBytes[Char].hashBytes(Gid[A].tag.value),
        HashBytes[PosLong].hashBytes(Gid[A].isoPosLong.get(a))
      )

  given HashBytes[Timestamp] =
    HashBytes.by(_.toEpochMilli)

  given HashBytes[TimeSpan] =
    HashBytes.by(_.toMicroseconds)

  given HashBytes[Offset.P] =
    HashBytes.by(_.toAngle.toMicroarcseconds)

  given HashBytes[Offset.Q] =
    HashBytes.by(_.toAngle.toMicroarcseconds)

  given HashBytes[TelluricType] =
    case t @ TelluricType.Manual(starTypes) =>
      Array.concat(
        HashBytes[String].hashBytes(t.tag),
        starTypes.toList.map(HashBytes[String].hashBytes).flatten.toArray
      )
    case t                                  =>
      HashBytes[String].hashBytes(t.tag)

  given HashBytes[Offset] =
    HashBytes.by2(_.p, _.q)

  given HashBytes[Wavelength] =
    HashBytes.by(_.toPicometers.value)

  given HashBytes[SignalToNoise] with
    def hashBytes(s: SignalToNoise): Array[Byte] =
      Array.concat(
        toByteArray(BigInt(s.toBigDecimal.underlying().unscaledValue()), 11),
        s.toBigDecimal.scale.hashBytes
      )

  given HashBytes[ExposureTimeMode.SignalToNoiseMode] =
    HashBytes.by2(_.value, _.at)

  given HashBytes[ExposureTimeMode.TimeAndCountMode] with
    def hashBytes(t: ExposureTimeMode.TimeAndCountMode): Array[Byte] =
      Array.concat(
        t.time.hashBytes,
        t.count.hashBytes,
        t.at.hashBytes
      )

  given HashBytes[ExposureTimeMode] with
    def hashBytes(m: ExposureTimeMode): Array[Byte] =
      m match
        case s: ExposureTimeMode.SignalToNoiseMode => s.hashBytes
        case t: ExposureTimeMode.TimeAndCountMode  => t.hashBytes

  given [A](using HashBytes[A]): HashBytes[Option[A]] with
    def hashBytes(opt: Option[A]): Array[Byte] =
      opt.fold(Array.emptyByteArray)(HashBytes[A].hashBytes)

  given[A](using Enumerated[A]): HashBytes[A] with
    def hashBytes(a: A): Array[Byte] =
      Enumerated[A].tag(a).getBytes(UTF_8)