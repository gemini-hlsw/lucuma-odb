// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptySet
import cats.syntax.eq.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.odb.sequence.ObservingMode
import lucuma.core.enums.GmosRoi
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.core.enums.GmosRoi

/**
  * Defines logic used to determine the config items used for a given observing mode
  * and calibration type
  */
sealed trait CalibrationConfigMatcher:
  /** Supported calibration roles */
  def calibrationType: NonEmptySet[CalibrationRole]

  /** Supported calibration modes */
  def supportedModeTypes: NonEmptySet[ObservingModeType]

  /** config subset for the mode */
  def extractConfig(mode: ObservingMode): CalibrationConfigSubset

  /**
   * Check if two configurations match for this calibration type.
   * Different calibration types may have different matching semantics.
   * e.g. spec photo doesn't care if roi are different.
   */
  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean

  /**
   * Normalize a configuration for comparison purposes.
   * This allows different calibration types to apply their specific transformations.
   */
  def normalize(config: CalibrationConfigSubset): CalibrationConfigSubset

object CalibrationConfigMatcher:

  private val strategies: Map[(ObservingModeType, CalibrationRole), CalibrationConfigMatcher] = Map(
    (ObservingModeType.GmosNorthLongSlit, CalibrationRole.SpectroPhotometric) -> SpecphotoGmosLS,
    (ObservingModeType.GmosSouthLongSlit, CalibrationRole.SpectroPhotometric) -> SpecphotoGmosLS,
    (ObservingModeType.GmosNorthLongSlit, CalibrationRole.Twilight) -> TwilightGmosLS,
    (ObservingModeType.GmosSouthLongSlit, CalibrationRole.Twilight) -> TwilightGmosLS
  )

  def getSupportedCalibrationTypes(modeType: ObservingModeType): Set[CalibrationRole] =
    strategies.keys.filter(_._1 == modeType).map(_._2).toSet

  def getStrategyForComparison(config: CalibrationConfigSubset, calibRole: CalibrationRole): Option[CalibrationConfigMatcher] =
    val modeType = config match
      case _: GmosNConfigs => ObservingModeType.GmosNorthLongSlit
      case _: GmosSConfigs => ObservingModeType.GmosSouthLongSlit
      case _: GmosNImagingConfigs => ObservingModeType.GmosNorthImaging
      case _: GmosSImagingConfigs => ObservingModeType.GmosSouthImaging
      case _: Flamingos2Configs => ObservingModeType.Flamingos2LongSlit
    strategies.get((modeType, calibRole))

object SpecphotoGmosLS extends CalibrationConfigMatcher:
  import lucuma.odb.sequence.gmos.longslit.Config

  def extractConfig(mode: ObservingMode): CalibrationConfigSubset =
    mode match
      case gn: Config.GmosNorth =>
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
      case gs: Config.GmosSouth =>
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

  def calibrationType: NonEmptySet[CalibrationRole] =
    NonEmptySet.of(CalibrationRole.SpectroPhotometric)

  def supportedModeTypes: NonEmptySet[ObservingModeType] =
    NonEmptySet.of(ObservingModeType.GmosNorthLongSlit, ObservingModeType.GmosSouthLongSlit)

  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean =
    normalize(c1) === normalize(c2)

  def normalize(config: CalibrationConfigSubset): CalibrationConfigSubset =
    config match
      case gn: GmosNConfigs => gn.copy(roi = GmosRoi.CentralSpectrum)
      case gs: GmosSConfigs => gs.copy(roi = GmosRoi.CentralSpectrum)
      case other => other

object TwilightGmosLS extends CalibrationConfigMatcher:
  import lucuma.odb.sequence.gmos.longslit.Config

  def extractConfig(mode: ObservingMode): CalibrationConfigSubset =
    mode match
      case gn: Config.GmosNorth =>
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
      case gs: Config.GmosSouth =>
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

  def calibrationType: NonEmptySet[CalibrationRole] = NonEmptySet.of(CalibrationRole.Twilight)

  def supportedModeTypes: NonEmptySet[ObservingModeType] =
    NonEmptySet.of(ObservingModeType.GmosNorthLongSlit, ObservingModeType.GmosSouthLongSlit)

  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean =
    c1 === c2

  def normalize(config: CalibrationConfigSubset): CalibrationConfigSubset =
    config
