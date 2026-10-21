// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.Eq
import lucuma.core.enums.DaytimeCalibrationObservingModeType
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.odb.sequence.util.HashBytes

/**
 * Configuration for a daytime calibration: the instrument and the ordered
 * calibration sets that generate its sequence.
 */
final case class Config(
  instrument: Instrument,
  sets:       List[CalibrationSet[InstrumentSetup]]
):

  def mode: DaytimeCalibrationObservingModeType =
    DaytimeCalibrationObservingModeType.DaytimeCalibration

  def gmosNorthSets: List[CalibrationSet[DynamicConfig.GmosNorth]] =
    sets.collect:
      case s @ CalibrationSet(_, _, _, InstrumentSetup.GmosNorth(c)) => s.copy(setup = c)

  def gmosSouthSets: List[CalibrationSet[DynamicConfig.GmosSouth]] =
    sets.collect:
      case s @ CalibrationSet(_, _, _, InstrumentSetup.GmosSouth(c)) => s.copy(setup = c)

object Config:

  given Eq[Config] =
    Eq.by(c => (c.instrument, c.sets))

  given HashBytes[Config] with
    def hashBytes(c: Config): Array[Byte] =
      Array.concat(
        HashBytes[Instrument].hashBytes(c.instrument),
        HashBytes[List[CalibrationSet[InstrumentSetup]]].hashBytes(c.sets)
      )
