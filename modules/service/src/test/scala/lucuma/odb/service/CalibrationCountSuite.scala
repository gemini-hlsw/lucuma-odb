// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import munit.FunSuite

// Boundary arithmetic only; the mode and role gating is covered end to end in
// executionDigest_calibrationCount and executionDigest_calibrationCountGnirs.
class CalibrationCountSuite extends FunSuite:

  private def minutes(m: Long): TimeSpan =
    TimeSpan.unsafeFromMicroseconds(m * 60_000_000L)

  private def nm(n: Int): Wavelength =
    Wavelength.fromIntNanometers(n).get

  test("one set per interval, rounded up"):
    val i = ObsExtract.ShortWavelengthSetInterval
    assertEquals(ObsExtract.calibrationSets(i, minutes(0)).value, 0)
    assertEquals(ObsExtract.calibrationSets(i, minutes(60)).value, 1)
    assertEquals(ObsExtract.calibrationSets(i, minutes(90)).value, 1)
    assertEquals(ObsExtract.calibrationSets(i, minutes(91)).value, 2)
    assertEquals(ObsExtract.calibrationSets(i, minutes(300)).value, 4)

  test("the long-wavelength interval is one hour"):
    val i = ObsExtract.LongWavelengthSetInterval
    assertEquals(ObsExtract.calibrationSets(i, minutes(60)).value, 1)
    assertEquals(ObsExtract.calibrationSets(i, minutes(61)).value, 2)
    assertEquals(ObsExtract.calibrationSets(i, minutes(300)).value, 5)

  test("the interval is 90 minutes strictly below 2.6 µm, 60 minutes from 2.6 µm up"):
    val short = ObsExtract.ShortWavelengthSetInterval
    val long  = ObsExtract.LongWavelengthSetInterval
    assertEquals(ObsExtract.calibrationSetInterval(nm(2200)), short)
    assertEquals(ObsExtract.calibrationSetInterval(nm(2599)), short)
    assertEquals(ObsExtract.calibrationSetInterval(nm(2600)), long)
    assertEquals(ObsExtract.calibrationSetInterval(nm(3400)), long)

  // The count is an estimate; the group still holds at most two tellurics.
  test("ten hours of science: seven sets below 2.6 µm, ten above, two tellurics per visit"):
    val tenHours = minutes(600)
    val short    = ObsExtract.ShortWavelengthSetInterval
    val long     = ObsExtract.LongWavelengthSetInterval
    assertEquals(ObsExtract.calibrationSets(short, tenHours).value, 7)
    assertEquals(ObsExtract.calibrationSets(long, tenHours).value, 10)
    assertEquals(ObsExtract.telluricsForVisit(tenHours).value, 2)

  test("tellurics per visit: one up to the threshold, two beyond"):
    assertEquals(ObsExtract.telluricsForVisit(minutes(90)).value, 1)
    assertEquals(ObsExtract.telluricsForVisit(minutes(91)).value, 2)
