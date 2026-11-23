// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.Eq
import cats.syntax.eq.*
import lucuma.core.enums.DaytimeCalibrationObservingModeType
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.sequence.util.HashBytes

/**
 * Configuration for a daytime calibration: the instrument, the ordered
 * calibration sets that generate its sequence and the one setting its
 * instrument shares across them: the resolution mode for GHOST, the well
 * depth for GNIRS.
 */
final case class Config(
  instrument:          Instrument,
  sets:                List[CalibrationSet[InstrumentSetup]],
  ghostResolutionMode: Option[GhostResolutionMode] = None,
  gnirsWellDepth:      Option[GnirsWellDepth]      = None
):

  def mode: DaytimeCalibrationObservingModeType =
    DaytimeCalibrationObservingModeType.DaytimeCalibration

  def flamingos2Sets: List[CalibrationSet[Flamingos2DynamicConfig]] =
    sets.collect:
      case s @ CalibrationSet(setup = InstrumentSetup.Flamingos2(c)) => s.copy(setup = c)

  def ghostStatic: GhostStaticConfig =
    GhostStaticConfig(ghostResolutionMode.getOrElse(Config.DefaultGhostResolutionMode), GhostIfuMapping.NoTarget, None)

  def gnirsStatic: GnirsStaticConfig =
    GnirsStaticConfig(gnirsWellDepth.getOrElse(Config.DefaultGnirsWellDepth))

  def ghostSets: List[CalibrationSet[GhostDynamicConfig]] =
    sets.collect:
      case s @ CalibrationSet(setup = InstrumentSetup.Ghost(c)) => s.copy(setup = c)

  def gmosNorthSets: List[CalibrationSet[DynamicConfig.GmosNorth]] =
    sets.collect:
      case s @ CalibrationSet(setup = InstrumentSetup.GmosNorth(c)) => s.copy(setup = c)

  def gmosSouthSets: List[CalibrationSet[DynamicConfig.GmosSouth]] =
    sets.collect:
      case s @ CalibrationSet(setup = InstrumentSetup.GmosSouth(c)) => s.copy(setup = c)

  def gnirsSets: List[CalibrationSet[GnirsDynamicConfig]] =
    sets.collect:
      case s @ CalibrationSet(setup = InstrumentSetup.Gnirs(c)) => s.copy(setup = c)

  def igrins2Sets: List[CalibrationSet[Igrins2DynamicConfig]] =
    sets.collect:
      case s @ CalibrationSet(setup = InstrumentSetup.Igrins2(c)) => s.copy(setup = c)

object Config:

  val DefaultGhostResolutionMode: GhostResolutionMode =
    GhostResolutionMode.Standard

  /** The resolution mode a daytime calibration of this instrument holds: one for GHOST, none otherwise. */
  def ghostResolutionModeFor(instrument: Instrument, requested: Option[GhostResolutionMode]): Option[GhostResolutionMode] =
    Option.when(instrument === Instrument.Ghost)(requested.getOrElse(DefaultGhostResolutionMode))

  val DefaultGnirsWellDepth: GnirsWellDepth =
    GnirsWellDepth.Shallow

  /** The well depth a daytime calibration of this instrument holds: one for GNIRS, none otherwise. */
  def gnirsWellDepthFor(instrument: Instrument, requested: Option[GnirsWellDepth]): Option[GnirsWellDepth] =
    Option.when(instrument === Instrument.Gnirs)(requested.getOrElse(DefaultGnirsWellDepth))

  given Eq[Config] =
    Eq.by(c => (c.instrument, c.sets, c.ghostResolutionMode, c.gnirsWellDepth))

  given HashBytes[Config] with
    def hashBytes(c: Config): Array[Byte] =
      Array.concat(
        HashBytes[Instrument].hashBytes(c.instrument),
        HashBytes[List[CalibrationSet[InstrumentSetup]]].hashBytes(c.sets),
        HashBytes[Option[GhostResolutionMode]].hashBytes(c.ghostResolutionMode),
        HashBytes[Option[GnirsWellDepth]].hashBytes(c.gnirsWellDepth)
      )
