// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.eq.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.ObservingModeType
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.service.CalibrationConfigSubset.*

/**
  * Defines logic used to determine the config items used for a given observing mode
  * and calibration type
  */
sealed trait CalibrationConfigMatcher:
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
  private val UnknownConfig: CalibrationConfigMatcher =
    new CalibrationConfigMatcher {
      def extractConfig(mode: ObservingMode): CalibrationConfigSubset =
        mode.toConfigSubset
      def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean =
        false
      def normalize(config: CalibrationConfigSubset): CalibrationConfigSubset =
        config
    }

  private def matchers(om: ObservingModeType, role: CalibrationRole): CalibrationConfigMatcher =
    (om, role) match
      case (_, CalibrationRole.SpectroPhotometric) => SpecphotoGmosLS
      case (_, CalibrationRole.Twilight)           => TwilightGmosLS
      case (_, _)                                  => UnknownConfig

  def matcherFor(config: CalibrationConfigSubset, calibRole: CalibrationRole): CalibrationConfigMatcher =
    val modeType = config match
      case _: GmosNConfigs        => ObservingModeType.GmosNorthLongSlit
      case _: GmosSConfigs        => ObservingModeType.GmosSouthLongSlit
      case _: GmosNImagingConfigs => ObservingModeType.GmosNorthImaging
      case _: GmosSImagingConfigs => ObservingModeType.GmosSouthImaging
      case _: Flamingos2Configs   => ObservingModeType.Flamingos2LongSlit
    matchers(modeType, calibRole)

object SpecphotoGmosLS extends CalibrationConfigMatcher:
  def extractConfig(mode: ObservingMode): CalibrationConfigSubset =
    normalize(mode.toConfigSubset)

  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean =
    normalize(c1) === normalize(c2)

  def normalize(config: CalibrationConfigSubset): CalibrationConfigSubset =
    config match
      case gn: GmosNConfigs => gn.copy(roi = GmosRoi.CentralSpectrum)
      case gs: GmosSConfigs => gs.copy(roi = GmosRoi.CentralSpectrum)
      case other => other

object TwilightGmosLS extends CalibrationConfigMatcher:
  def extractConfig(mode: ObservingMode): CalibrationConfigSubset =
    mode.toConfigSubset

  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean =
    c1 === c2

  def normalize(config: CalibrationConfigSubset): CalibrationConfigSubset =
    config
