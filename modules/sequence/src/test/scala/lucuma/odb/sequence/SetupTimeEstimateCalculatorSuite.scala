// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.core.model.sequence.SetupTime
import lucuma.core.util.TimeSpan
import munit.FunSuite

class SetupTimeEstimateCalculatorSuite extends FunSuite:

  private val pwfs: SetupTimeEstimateCalculator =
    SetupTimeEstimateCalculator.pwfsSpectroscopy(
      SetupTime(TimeSpan.fromMinutes(16).get, TimeSpan.fromMinutes(5).get)
    )

  private def counts(minutes: Int): (Int, Int) =
    val t = TimeSpan.fromMinutes(minutes).get
    (pwfs.estimateSetupCount(t).value, pwfs.estimateReacquisitionCount(t).value)

  test("PWFS spectroscopy setup and reacquisition counts"):
    // setups = floor(t / 120) + 1, reacquisitions = floor((t + 60) / 120)
    assertEquals(counts(0),   (0, 0))
    assertEquals(counts(59),  (1, 0))
    assertEquals(counts(60),  (1, 1))
    assertEquals(counts(119), (1, 1))
    assertEquals(counts(120), (2, 1))
    assertEquals(counts(179), (2, 1))
    assertEquals(counts(180), (2, 2))
    assertEquals(counts(240), (3, 2))

  test("PWFS spectroscopy total setup time"):
    // 3 hours: 2 setups (32 min) + 2 reacquisitions (10 min)
    assertEquals(pwfs.totalSetupTime(TimeSpan.fromMinutes(180).get), TimeSpan.fromMinutes(42).get)
