// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.data.CalibrationGroupTelluric
import lucuma.odb.sequence.data.CalibrationGroupTellurics
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

  // The mode and role gating is covered end to end in executionDigest_calibrationEstimate.
  test("expected calibrations charge each telluric still to come"):
    def telluric(unobserved: Boolean, total: Option[Long], declined: Boolean = false): CalibrationGroupTelluric =
      CalibrationGroupTelluric(
        unobserved,
        declined,
        total.map(m => CategorizedTime(ChargeClass.Program -> minutes(m)))
      )
    def estimate(count: Int, tellurics: CalibrationGroupTelluric*) =
      val e = ObsExtract.telluricEstimate(
        NonNegInt.unsafeFrom(count),
        CalibrationGroupTellurics(tellurics.toList)
      )
      (e.expected.count.value, e.expected.time(ChargeClass.Program).toMinutes.toLong)
    // The placeholder until a telluric has a digest, then the average.
    assertEquals(estimate(7), (7, 105L))
    assertEquals(estimate(7, telluric(true, None), telluric(true, None)), (5, 75L))
    assertEquals(estimate(7, telluric(true, Some(30)), telluric(true, Some(50))), (5, 200L))
    assertEquals(estimate(1, telluric(true, Some(40)), telluric(true, Some(40))), (0, 0L))
    assertEquals(estimate(0), (0, 0L))
    // A declined telluric fills its slot but does not change the average.
    assertEquals(estimate(7, telluric(true, Some(40), declined = true)), (6, 90L))
    assertEquals(estimate(3, telluric(true, Some(40)), telluric(true, Some(60), declined = true)), (1, 40L))
    // Observed tellurics are not subtracted, but their totals feed the average.
    assertEquals(estimate(7, telluric(false, Some(40))), (7, 280L))

  test("existing calibrations are the unobserved active ones, each at its own estimate or the placeholder"):
    val group = CalibrationGroupTellurics(List(
      CalibrationGroupTelluric(true,  false, Some(CategorizedTime(ChargeClass.Program -> minutes(40)))),
      CalibrationGroupTelluric(true,  false, None),
      CalibrationGroupTelluric(true,  true,  Some(CategorizedTime(ChargeClass.Program -> minutes(90)))),
      CalibrationGroupTelluric(false, false, Some(CategorizedTime(ChargeClass.Program -> minutes(20))))
    ))
    val e = ObsExtract.telluricEstimate(NonNegInt.unsafeFrom(4), group)
    assertEquals(e.existing.count.value, 2)
    assertEquals(e.existing.time(ChargeClass.Program).toMinutes.toLong, 55L)
    assertEquals(e.expected.count.value, 1)
    assertEquals(e.expected.time(ChargeClass.Program).toMinutes.toLong, 30L)
    assertEquals(e.count.value, 3)

  test("the unit cost is the mean of the tellurics per charge class"):
    val a = CategorizedTime(ChargeClass.Program -> minutes(30), ChargeClass.NonCharged -> minutes(2))
    val b = CategorizedTime(ChargeClass.Program -> minutes(50), ChargeClass.NonCharged -> minutes(4))
    assertEquals(CalibrationGroupTellurics.average(Nil), None)
    assertEquals(
      CalibrationGroupTellurics.average(List(a, b)),
      Some(CategorizedTime(ChargeClass.Program -> minutes(40), ChargeClass.NonCharged -> minutes(3)))
    )

  test("tellurics per visit: one up to the threshold, two beyond"):
    assertEquals(ObsExtract.telluricsForVisit(minutes(90)).value, 1)
    assertEquals(ObsExtract.telluricsForVisit(minutes(91)).value, 2)
