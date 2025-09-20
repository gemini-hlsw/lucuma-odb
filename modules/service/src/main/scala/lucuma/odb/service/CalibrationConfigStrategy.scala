// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptySet
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.odb.sequence.ObservingMode

/**
  * Defines logic used to determine the config items used for a given observing mode
  * and calibration type
  */
trait CalibrationConfigStrategy:
  def calibrationType: CalibrationRole
  def supportedModeTypes: NonEmptySet[ObservingModeType]

  def extractConfig(mode: ObservingMode): CalibrationConfigSubset

  /**
   * Check if two configurations match for this calibration type.
   * Different calibration types may have different matching semantics.
   */
  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean

  /**
   * Normalize a configuration for comparison purposes.
   * This allows different calibration types to apply their specific transformations.
   */
  def normalizeForComparison(config: CalibrationConfigSubset): CalibrationConfigSubset

object CalibrationConfigStrategy:

  private val strategies: Map[(ObservingModeType, CalibrationRole), CalibrationConfigStrategy] = Map(
    (ObservingModeType.GmosNorthLongSlit, CalibrationRole.SpectroPhotometric) -> SpecphotoGmosLS,
    (ObservingModeType.GmosSouthLongSlit, CalibrationRole.SpectroPhotometric) -> SpecphotoGmosLS,
    (ObservingModeType.GmosNorthLongSlit, CalibrationRole.Twilight) -> TwilightGmosLS,
    (ObservingModeType.GmosSouthLongSlit, CalibrationRole.Twilight) -> TwilightGmosLS
  )

  def getStrategy(modeType: ObservingModeType, calibRole: CalibrationRole): Option[CalibrationConfigStrategy] =
    strategies.get((modeType, calibRole))

  def getSupportedCalibrationTypes(modeType: ObservingModeType): Set[CalibrationRole] =
    strategies.keys.filter(_._1 == modeType).map(_._2).toSet

  def getStrategyForComparison(config: CalibrationConfigSubset, calibRole: CalibrationRole): Option[CalibrationConfigStrategy] =
    import CalibrationConfigSubset.*
    val modeType = config match
      case _: GmosNConfigs => ObservingModeType.GmosNorthLongSlit
      case _: GmosSConfigs => ObservingModeType.GmosSouthLongSlit
      case _: GmosNImagingConfigs => ObservingModeType.GmosNorthImaging
      case _: GmosSImagingConfigs => ObservingModeType.GmosSouthImaging
      case _: Flamingos2Configs => ObservingModeType.Flamingos2LongSlit
    getStrategy(modeType, calibRole)

object SpecphotoGmosLS extends CalibrationConfigStrategy:
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

  def supportedModeTypes: NonEmptySet[ObservingModeType] = NonEmptySet.of(
    ObservingModeType.GmosNorthLongSlit,
    ObservingModeType.GmosSouthLongSlit
  )

  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean =
    normalizeForComparison(c1) == normalizeForComparison(c2)

  def normalizeForComparison(config: CalibrationConfigSubset): CalibrationConfigSubset =
    import CalibrationConfigSubset.*
    import lucuma.core.enums.GmosRoi
    config match
      case gn: GmosNConfigs => gn.copy(roi = GmosRoi.CentralSpectrum)
      case gs: GmosSConfigs => gs.copy(roi = GmosRoi.CentralSpectrum)
      case other => other

object TwilightGmosLS extends CalibrationConfigStrategy:
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

  def supportedModeTypes: NonEmptySet[ObservingModeType] = NonEmptySet.of(
    ObservingModeType.GmosNorthLongSlit,
    ObservingModeType.GmosSouthLongSlit
  )

  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean =
    c1 == c2

  def normalizeForComparison(config: CalibrationConfigSubset): CalibrationConfigSubset =
    config
