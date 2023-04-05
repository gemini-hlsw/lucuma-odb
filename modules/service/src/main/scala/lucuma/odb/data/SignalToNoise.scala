// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.Decoder
import io.circe.Encoder
import io.circe.JsonNumber

// Signal-to-noise stored as milli-signal to noise Long capped at 9,999,999,999
// so that it fits in a database numeric(10, 3).

opaque type SignalToNoise = Long

object SignalToNoise {

  /**
   * Maximum supported signal-to-noise value: 9,999,999.999.
   */
  val Max: SignalToNoise =
    9_999_999_999L

  /**
   * Minimum supported signal-to-noise value: 0.001.
   */
  val Min: SignalToNoise =
    1L

  extension (s2n: SignalToNoise) {

    /**
     * Converts this SignalToNoise value to a `BigDecimal`, which is guaranteed
     * to be positive.
     */
    def toBigDecimal: BigDecimal =
      BigDecimal(s2n, 3)

    /**
     * Converts this SignalToNoise value to a `PosBigDecimal`.
     */
    def toPosBigDecimal: PosBigDecimal =
      PosBigDecimal.unsafeFrom(toBigDecimal)

  }

  private def fromMilliLong(msn: Long): Option[SignalToNoise] =
    Option.when(msn >= Min && msn <= Max)(msn)

  private def unsafeFromMilliLong(msn: Long): SignalToNoise =
    fromMilliLong(msn).get

  private def fromMilliDecimal(msn: BigDecimal): Option[SignalToNoise] =
    Option.when(msn.isValidLong)(msn.longValue).flatMap(fromMilliLong)

  private def errorMessage(sn: BigDecimal): String =
    s"Signal-to-noise is limited to [${Min.toBigDecimal}, ${Max.toBigDecimal}), got $sn"

  private def error(sn: BigDecimal): Nothing =
    sys.error(errorMessage(sn))

  /**
   * Creates a `SignalToNoise` value assuming that the given BigDecimal is in
   * range [Min, Max] and does not have a finer scale than milli-sn.
   */
  def fromBigDecimalExact(sn: BigDecimal): Option[SignalToNoise] =
    fromMilliDecimal(sn * 1000)

  def unsafeFromBigDecimalExact(sn: BigDecimal): SignalToNoise =
    fromBigDecimalExact(sn).getOrElse(error(sn))

  /**
   * Creates a `SignalToNoise` value assuming that the given BigDecimal is in
   * range [Min, Max].  Rounds finer scale values to milli-sn.
   */
  def fromBigDecimalRounding(sn: BigDecimal): Option[SignalToNoise] =
    fromMilliDecimal((sn * 1000).setScale(0, BigDecimal.RoundingMode.HALF_UP))

  def unsafeFromBigDecimalRounding(sn: BigDecimal): SignalToNoise =
    fromBigDecimalRounding(sn).getOrElse(error(sn))

  /**
   * Creates a `SignalToNoise` value from the given integer assuming it is
   * positive non-zero and less than 10,000,000.
   */
  def fromInt(sn: Int): Option[SignalToNoise] =
    fromMilliLong(sn * 1000L)

  def unsafeFromInt(sn: Int): SignalToNoise =
    fromInt(sn).getOrElse(error(BigDecimal(sn)))  

  given Order[SignalToNoise] =
    Order.fromLessThan(_ < _)

  given Decoder[SignalToNoise] =
    Decoder.decodeJsonNumber.emap { jNum =>
      for {
        bd <- jNum.toBigDecimal.toRight(s"Signal-to-noise values must be parsable as a decimal, not: $jNum")
        sn <- fromBigDecimalExact(bd).toRight(errorMessage(bd))
      } yield sn
    }

  given Encoder[SignalToNoise] =
    Encoder.encodeJsonNumber.contramap[SignalToNoise] { sn =>
      JsonNumber.fromDecimalStringUnsafe(
        sn.toBigDecimal.underlying.toPlainString
      )
    }

}
