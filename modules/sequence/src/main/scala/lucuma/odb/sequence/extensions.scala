// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState

extension(role: Option[CalibrationRole])
  // what is the observe claass for the science on a particular calibration role
  def sciClass: ObserveClass =
    role match
      case Some(CalibrationRole.Twilight)            => ObserveClass.DayCal
      case Some(CalibrationRole.DaytimePinhole)      => ObserveClass.DayCal
      case Some(CalibrationRole.Telluric)            => ObserveClass.NightCal
      case Some(CalibrationRole.SpectroPhotometric)  => ObserveClass.NightCal
      case _                                         => ObserveClass.Science

  // what is the observa claass for gcal steps for a particular calibration role
  def gcalClass: ObserveClass =
    role match
      case Some(CalibrationRole.Twilight)            => ObserveClass.DayCal
      case Some(CalibrationRole.DaytimePinhole)      => ObserveClass.DayCal
      case _                                         => ObserveClass.NightCal

  // whether science steps are guided for a particular calibration role.  this
  // mirrors GuideService.GuideEnvironment.CalRolesWithoutGuiding, which skips
  // guide star selection altogether for these roles
  def sciGuiding: StepGuideState =
    role match
      case Some(CalibrationRole.Twilight)            => StepGuideState.Disabled
      case Some(CalibrationRole.DaytimePinhole)      => StepGuideState.Disabled
      case _                                         => StepGuideState.Enabled
