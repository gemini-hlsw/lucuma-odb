// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType

object CalibrationConfigRegistry:

  private val strategies: Map[(ObservingModeType, CalibrationRole), CalibrationConfigStrategy] = Map(
    (ObservingModeType.GmosNorthLongSlit, CalibrationRole.SpectroPhotometric) -> SpectroPhotometricGmosLongSlitStrategy,
    (ObservingModeType.GmosSouthLongSlit, CalibrationRole.SpectroPhotometric) -> SpectroPhotometricGmosLongSlitStrategy,
    (ObservingModeType.GmosNorthLongSlit, CalibrationRole.Twilight) -> TwilightGmosLongSlitStrategy,
    (ObservingModeType.GmosSouthLongSlit, CalibrationRole.Twilight) -> TwilightGmosLongSlitStrategy
  )

  def getStrategy(modeType: ObservingModeType, calibRole: CalibrationRole): Option[CalibrationConfigStrategy] =
    strategies.get((modeType, calibRole))

  def getSupportedCalibrationTypes(modeType: ObservingModeType): Set[CalibrationRole] =
    strategies.keys.filter(_._1 == modeType).map(_._2).toSet

  def registerStrategy(modeType: ObservingModeType, calibRole: CalibrationRole, strategy: CalibrationConfigStrategy): Unit =
    // For now, this would require modifying the strategies map
    // In a more dynamic system, this could be a mutable map
    throw new UnsupportedOperationException("Dynamic strategy registration not implemented")