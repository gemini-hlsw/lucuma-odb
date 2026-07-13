// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.exchange

import cats.Eq
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.KeckInstrument
import lucuma.core.enums.Observatory
import lucuma.core.enums.SubaruInstrument
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.util.HashBytes

/**
 * Configuration for an "exchange" observation, in which a Gemini PI requests
 * time at another observatory (Keck or Subaru).  These observations are not
 * supported by the ITC or AGS, so there is no sequence; we capture only the
 * exchange instrument and the total requested time.
 */
final case class Config(
  instrument:       Either[KeckInstrument, SubaruInstrument],
  totalRequestTime: TimeSpan
):
  def mode: ExchangeObservingModeType =
    instrument.fold(_ => ExchangeObservingModeType.ExchangeKeck, _ => ExchangeObservingModeType.ExchangeSubaru)

  def observatory: Observatory =
    mode match
      case ExchangeObservingModeType.ExchangeKeck   => Observatory.Keck
      case ExchangeObservingModeType.ExchangeSubaru => Observatory.Subaru

  def keckInstrument: Option[KeckInstrument] =
    instrument.swap.toOption

  def subaruInstrument: Option[SubaruInstrument] =
    instrument.toOption

object Config:

  given Eq[Config] =
    Eq.by(a => (a.instrument, a.totalRequestTime))

  given HashBytes[Config] with
    def hashBytes(c: Config): Array[Byte] =
      Array.concat(
        c.keckInstrument.fold(Array.emptyByteArray)(HashBytes[KeckInstrument].hashBytes),
        c.subaruInstrument.fold(Array.emptyByteArray)(HashBytes[SubaruInstrument].hashBytes),
        HashBytes[TimeSpan].hashBytes(c.totalRequestTime)
      )