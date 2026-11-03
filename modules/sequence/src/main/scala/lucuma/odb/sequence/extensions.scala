// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.eq.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState

extension(role: Option[CalibrationRole])
  def isDayCalibration: Boolean =
    role.exists(_.observeClass === ObserveClass.DayCal)

  // observe class for the science steps of a particular calibration role
  def sciClass: ObserveClass =
    role.fold(ObserveClass.Science)(_.observeClass)

  // observe class for the gcal steps of a particular calibration role
  def gcalClass: ObserveClass =
    role.fold(ObserveClass.NightCal)(_.observeClass)

  def sciGuiding: StepGuideState =
    if isDayCalibration then StepGuideState.Disabled else StepGuideState.Enabled
