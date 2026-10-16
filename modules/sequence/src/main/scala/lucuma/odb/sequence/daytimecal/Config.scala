// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.Eq
import lucuma.core.enums.DaytimeCalibrationObservingModeType
import lucuma.core.enums.Instrument
import lucuma.odb.sequence.util.HashBytes

/**
 * Configuration for a daytime calibration.  The mode's only parameter is the instrument.
 */
final case class Config(
  instrument: Instrument
):
  def mode: DaytimeCalibrationObservingModeType =
    DaytimeCalibrationObservingModeType.DaytimeCalibration

object Config:

  given Eq[Config] =
    Eq.by(_.instrument)

  given HashBytes[Config] with
    def hashBytes(c: Config): Array[Byte] =
      HashBytes[Instrument].hashBytes(c.instrument)
