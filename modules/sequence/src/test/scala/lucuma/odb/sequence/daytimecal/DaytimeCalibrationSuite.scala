// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
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

  private def atom(title: String, steps: ProtoStep[Int]*): ProtoAtom[ProtoStep[Int]] =
    ProtoAtom(NonEmptyString.from(title).toOption, NonEmptyList.fromListUnsafe(steps.toList))

  test("no atoms stay no atoms"):
    assertEquals(DaytimeCalibration.singleAtom(List.empty[ProtoAtom[ProtoStep[Int]]]), Nil)

  test("the steps of every atom are concatenated in order under the fixed description"):
    val dark1: ProtoStep[Int] = step(1, StepConfig.Dark)
    val dark2: ProtoStep[Int] = step(2, StepConfig.Dark)
    val bias:  ProtoStep[Int] = step(3, StepConfig.Bias)
    val atoms: List[ProtoAtom[ProtoStep[Int]]] =
      List(atom("Dark x2", dark1, dark1), atom("Bias x1", bias), atom("Dark x1", dark2))

    assertEquals(
      DaytimeCalibration.singleAtom(atoms),
      List(ProtoAtom(DaytimeCalibration.Description.some, NonEmptyList.of(dark1, dark1, bias, dark2)))
    )

  test("a single atom is only retitled"):
    val bias: ProtoStep[Int] = step(1, StepConfig.Bias)
    assertEquals(
      DaytimeCalibration.singleAtom(List(atom("Bias x1", bias))),
      List(atom(DaytimeCalibration.Description.value, bias))
    )

  test("the description"):
    assertEquals(DaytimeCalibration.Description.value, "Daytime Calibration")
