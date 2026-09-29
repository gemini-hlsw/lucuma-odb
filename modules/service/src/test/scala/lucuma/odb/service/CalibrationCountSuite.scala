// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.data.TelluricSiblings
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

  // The mode and role gating is covered end to end in executionDigest_expectedCalibrations.
  test("expected calibrations charge each telluric still to come"):
    def program(count: Int, unobserved: Int, declined: Boolean, unit: Option[Long]): Long =
      val cost     = unit.map(m => CategorizedTime(ChargeClass.Program -> minutes(m)))
      val siblings = TelluricSiblings(NonNegInt.unsafeFrom(unobserved), declined, cost)
      ObsExtract
        .expectedTelluricTime(NonNegInt.unsafeFrom(count), siblings)
        .apply(ChargeClass.Program)
        .toMinutes
        .toLong
    // The placeholder until a telluric has a digest, then the average.
    assertEquals(program(7, 0, false, None), 105L)
    assertEquals(program(7, 2, false, None), 75L)
    assertEquals(program(7, 2, false, Some(40)), 200L)
    assertEquals(program(7, 9, false, Some(40)), 0L)
    assertEquals(program(0, 0, false, None), 0L)
    assertEquals(program(7, 0, true, Some(40)), 0L)

  test("the unit cost is the mean of the tellurics per charge class"):
    val a = CategorizedTime(ChargeClass.Program -> minutes(30), ChargeClass.NonCharged -> minutes(2))
    val b = CategorizedTime(ChargeClass.Program -> minutes(50), ChargeClass.NonCharged -> minutes(4))
    assertEquals(TelluricSiblings.average(Nil), None)
    assertEquals(
      TelluricSiblings.average(List(a, b)),
      Some(CategorizedTime(ChargeClass.Program -> minutes(40), ChargeClass.NonCharged -> minutes(3)))
    )

  test("tellurics per visit: one up to the threshold, two beyond"):
    assertEquals(ObsExtract.telluricsForVisit(minutes(90)).value, 1)
    assertEquals(ObsExtract.telluricsForVisit(minutes(91)).value, 2)
