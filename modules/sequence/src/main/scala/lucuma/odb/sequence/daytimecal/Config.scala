// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.Eq
import lucuma.core.enums.DayCalObservingModeType
import lucuma.core.enums.Instrument
import lucuma.odb.sequence.util.HashBytes

/**
 * Configuration for a daytime calibration: a staff-created calibration with
 * no target, ITC, guide star or acquisition.  The mode's only parameter is the
 * instrument; the sequence is entered by hand.
 */
final case class Config(
  instrument: Instrument
):
  def mode: DayCalObservingModeType =
    DayCalObservingModeType.DaytimeCalibration

object Config:

  /** Instruments whose sequences may be entered by hand.  GHOST is excluded: its static
    * config needs an IFU target mapping, which a daytime calibration cannot supply. */
  val AllowedInstruments: Set[Instrument] =
    Set(
      Instrument.Flamingos2,
      Instrument.GmosNorth,
      Instrument.GmosSouth,
      Instrument.Gnirs,
      Instrument.Igrins2
    )

  given Eq[Config] =
    Eq.by(_.instrument)

  given HashBytes[Config] with
    def hashBytes(c: Config): Array[Byte] =
      HashBytes[Instrument].hashBytes(c.instrument)
