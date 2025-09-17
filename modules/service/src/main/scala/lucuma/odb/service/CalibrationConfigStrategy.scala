// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.odb.sequence.ObservingMode

trait CalibrationConfigStrategy:
  def extractConfig(mode: ObservingMode): CalibrationConfigSubset
  def calibrationType: CalibrationRole
  def supportedModeTypes: Set[ObservingModeType]

object SpectroPhotometricGmosLongSlitStrategy extends CalibrationConfigStrategy:
  def extractConfig(mode: ObservingMode): CalibrationConfigSubset =
    import CalibrationConfigSubset.*
    import lucuma.core.enums.GmosRoi
    mode match
      case gn: lucuma.odb.sequence.gmos.longslit.Config.GmosNorth =>
        GmosNConfigs(
          gn.grating,
          gn.filter,
          gn.fpu,
          gn.centralWavelength,
          gn.xBin,
          gn.yBin,
          gn.ampReadMode,
          gn.ampGain,
          GmosRoi.CentralSpectrum
        )
      case gs: lucuma.odb.sequence.gmos.longslit.Config.GmosSouth =>
        GmosSConfigs(
          gs.grating,
          gs.filter,
          gs.fpu,
          gs.centralWavelength,
          gs.xBin,
          gs.yBin,
          gs.ampReadMode,
          gs.ampGain,
          GmosRoi.CentralSpectrum
        )
      case _ =>
        mode.toConfigSubset

  def calibrationType: CalibrationRole = CalibrationRole.SpectroPhotometric

  def supportedModeTypes: Set[ObservingModeType] = Set(
    ObservingModeType.GmosNorthLongSlit,
    ObservingModeType.GmosSouthLongSlit
  )

object TwilightGmosLongSlitStrategy extends CalibrationConfigStrategy:
  def extractConfig(mode: ObservingMode): CalibrationConfigSubset =
    import CalibrationConfigSubset.*
    mode match
      case gn: lucuma.odb.sequence.gmos.longslit.Config.GmosNorth =>
        GmosNConfigs(
          gn.grating,
          gn.filter,
          gn.fpu,
          gn.centralWavelength,
          gn.xBin,
          gn.yBin,
          gn.ampReadMode,
          gn.ampGain,
          gn.roi
        )
      case gs: lucuma.odb.sequence.gmos.longslit.Config.GmosSouth =>
        GmosSConfigs(
          gs.grating,
          gs.filter,
          gs.fpu,
          gs.centralWavelength,
          gs.xBin,
          gs.yBin,
          gs.ampReadMode,
          gs.ampGain,
          gs.roi
        )
      case _ =>
        mode.toConfigSubset

  def calibrationType: CalibrationRole = CalibrationRole.Twilight

  def supportedModeTypes: Set[ObservingModeType] = Set(
    ObservingModeType.GmosNorthLongSlit,
    ObservingModeType.GmosSouthLongSlit
  )