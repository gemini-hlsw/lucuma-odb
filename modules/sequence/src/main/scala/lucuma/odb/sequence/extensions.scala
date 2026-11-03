// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState

// Calibration roles taken as day calibrations: unguided, uncharged, DayCal class.
val DayCalibrationRoles: Set[CalibrationRole] =
  Set(
    CalibrationRole.DaytimeArc,
    CalibrationRole.DaytimeCalibration,
    CalibrationRole.DaytimeFlat,
    CalibrationRole.DaytimePinhole,
    CalibrationRole.Twilight
  )

extension(role: Option[CalibrationRole])
  def isDayCalibration: Boolean =
    role.exists(DayCalibrationRoles.contains)

  // observe class for the science steps of a particular calibration role
  def sciClass: ObserveClass =
    if isDayCalibration then ObserveClass.DayCal
    else role match
      case Some(CalibrationRole.SpectroPhotometric) |
           Some(CalibrationRole.Telluric)           => ObserveClass.NightCal
      case _                                        => ObserveClass.Science

  // observe class for the gcal steps of a particular calibration role
  def gcalClass: ObserveClass =
    if isDayCalibration then ObserveClass.DayCal else ObserveClass.NightCal

  def sciGuiding: StepGuideState =
    if isDayCalibration then StepGuideState.Disabled else StepGuideState.Enabled
