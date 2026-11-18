// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.data.NonEmptyList
import cats.syntax.option.*
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import munit.FunSuite

class DaytimeCalibrationSuite extends FunSuite:

  private def step(setup: Int, config: StepConfig): ProtoStep[Int] =
    ProtoStep(setup, config, TelescopeConfig(Offset.Zero, StepGuideState.Disabled), ObserveClass.DayCal)

  test("no steps give no atom"):
    assertEquals(DaytimeCalibration.singleAtom(List.empty[ProtoStep[Int]]), Nil)

  test("the steps are kept in order under the fixed description"):
    val dark1: ProtoStep[Int] = step(1, StepConfig.Dark)
    val dark2: ProtoStep[Int] = step(2, StepConfig.Dark)
    val bias: ProtoStep[Int]  = step(3, StepConfig.Bias)

    assertEquals(
      DaytimeCalibration.singleAtom(List(dark1, dark1, bias, dark2)),
      List(ProtoAtom(DaytimeCalibration.Description.some, NonEmptyList.of(dark1, dark1, bias, dark2)))
    )

  test("the description"):
    assertEquals(DaytimeCalibration.Description.value, "Daytime Calibration")
