// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.eq.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ObservingModeType.*
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
  val UnknownConfig: CalibrationConfigMatcher =
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
      case (GmosNorthLongSlit | GmosSouthLongSlit, CalibrationRole.SpectroPhotometric) =>
        SpecphotoGmosLS
      case (GmosNorthLongSlit | GmosSouthLongSlit, CalibrationRole.Twilight)           =>
        TwilightGmosLS
      case (Flamingos2LongSlit, CalibrationRole.Telluric)                              =>
        Flamingos2LS
      case (_, _)                                                                      =>
        UnknownConfig

  def matcherFor(config: CalibrationConfigSubset, calibRole: CalibrationRole): CalibrationConfigMatcher =
    val modeType = config match
      case _: GmosNConfigs        => GmosNorthLongSlit
      case _: GmosSConfigs        => GmosSouthLongSlit
      case _: GmosNImagingConfigs => GmosNorthImaging
      case _: GmosSImagingConfigs => GmosSouthImaging
      case _: Flamingos2Configs   => Flamingos2LongSlit
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

object Flamingos2LS extends CalibrationConfigMatcher:
  def extractConfig(mode: ObservingMode): CalibrationConfigSubset =
    mode.toConfigSubset

  def configsMatch(c1: CalibrationConfigSubset, c2: CalibrationConfigSubset): Boolean =
    c1 === c2

  def normalize(config: CalibrationConfigSubset): CalibrationConfigSubset =
    config
