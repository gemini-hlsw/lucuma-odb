// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Encoder
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
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

  given HashBytes[BigDecimal] with
    def hashBytes(bd: BigDecimal): Array[Byte] =
      Array.concat(
        toByteArray(BigInt(bd.underlying().unscaledValue()), 16),
        bd.scale.hashBytes
      )

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

  given HashBytes[Angle] =
    HashBytes.by(_.toMicroarcseconds)

  given HashBytes[Offset.P] =
    HashBytes.by(_.toAngle)

  given HashBytes[Offset.Q] =
    HashBytes.by(_.toAngle)

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

  given HashBytes[Coordinates] =
    HashBytes.by2(_.ra.toAngle, _.dec.toAngle)

  given HashBytes[TelescopeConfig] =
    HashBytes.by2(_.guiding, _.offset)

  given HashBytes[Wavelength] =
    HashBytes.by(_.toPicometers.value)

  given HashBytes[SignalToNoise] =
    HashBytes.by(_.toBigDecimal)

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

  given [A](using HashBytes[A]): HashBytes[List[A]] with
    def hashBytes(as: List[A]): Array[Byte] =
      val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
      val out: DataOutputStream      = new DataOutputStream(bao)

      as.foreach(a => out.write(a.hashBytes))

      out.close()
      bao.toByteArray

  given [A](using HashBytes[A]): HashBytes[NonEmptyList[A]] with
    def hashBytes(as: NonEmptyList[A]): Array[Byte] =
      as.toList.hashBytes

  given[A](using Enumerated[A]): HashBytes[A] with
    def hashBytes(a: A): Array[Byte] =
      Enumerated[A].tag(a).getBytes(UTF_8)
