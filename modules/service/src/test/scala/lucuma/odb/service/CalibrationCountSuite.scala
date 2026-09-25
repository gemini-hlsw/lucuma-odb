// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.util.TimeSpan
import munit.FunSuite

// Boundary arithmetic only; the mode and role gating is covered end to end in
// executionDigest_calibrationCount.
class CalibrationCountSuite extends FunSuite:

  private def minutes(m: Long): TimeSpan =
    TimeSpan.unsafeFromMicroseconds(m * 60_000_000L)

  test("one epoch per 90 minutes, rounded up"):
    assertEquals(ObsExtract.calibrationEpochs(minutes(0)).value, 0)
    assertEquals(ObsExtract.calibrationEpochs(minutes(60)).value, 1)
    assertEquals(ObsExtract.calibrationEpochs(minutes(90)).value, 1)
    assertEquals(ObsExtract.calibrationEpochs(minutes(91)).value, 2)
    assertEquals(ObsExtract.calibrationEpochs(minutes(300)).value, 4)

  test("tellurics per visit: one up to the threshold, two beyond"):
    assertEquals(ObsExtract.telluricsForVisit(minutes(90)).value, 1)
    assertEquals(ObsExtract.telluricsForVisit(minutes(91)).value, 2)
