// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow

import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState.Inactive
import lucuma.core.enums.ObservationWorkflowState.Ready
import lucuma.odb.service.ObservationWorkflowService.UserState
import munit.FunSuite

import ObservationValidationInfo.effectiveUserState

class EffectiveUserStateSuite extends FunSuite:

  val inactive: Option[UserState] = Some(Inactive)
  val ready: Option[UserState]    = Some(Ready)

  test("an ordinary observation has its own state"):
    assertEquals(effectiveUserState(None, inactive, ready), inactive)
    assertEquals(effectiveUserState(None, None, inactive), None)

  test("a calibration that isn't per-observation has its own state"):
    assertEquals(effectiveUserState(Some(CalibrationRole.SpectroPhotometric), None, inactive), None)

  test("a telluric inherits its science observation's state"):
    assertEquals(effectiveUserState(Some(CalibrationRole.Telluric), None, inactive), inactive)
    assertEquals(effectiveUserState(Some(CalibrationRole.Telluric), ready, None), None)

  test("a declined telluric stays inactive"):
    assertEquals(effectiveUserState(Some(CalibrationRole.Telluric), inactive, ready), inactive)

  test("a daytime pinhole always inherits, even if set inactive itself"):
    assertEquals(effectiveUserState(Some(CalibrationRole.DaytimePinhole), inactive, ready), ready)
    assertEquals(effectiveUserState(Some(CalibrationRole.DaytimePinhole), None, inactive), inactive)
