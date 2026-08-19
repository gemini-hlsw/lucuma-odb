// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.model

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.itc.ItcSeries
import lucuma.itc.ItcXAxis
import lucuma.itc.SeriesDataType
import munit.FunSuite

class ItcSeriesSuite extends FunSuite:

  private def series(xAxis: ItcXAxis, dataY: NonEmptyList[Double]): Option[ItcSeries] =
    ItcSeries.fromLegacy("title", SeriesDataType.FinalS2NData, dataY, xAxis)

  // 11 samples at 1 nm spacing, the first three at -2, -1 and 0 nm
  private val axis: ItcXAxis              = ItcXAxis(-2.0, 8.0, 11)
  private val dataY: NonEmptyList[Double] =
    NonEmptyList.fromListUnsafe(List(999.0, 998.0, 997.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0))

  test("ItcSeries.fromLegacy leaves a positive axis alone") {
    val x = ItcXAxis(1.0, 5.0, 5)
    val y = NonEmptyList.of(1.0, 2.0, 3.0, 4.0, 5.0)
    assertEquals(series(x, y).map(_.xAxis), x.some)
    assertEquals(series(x, y).map(_.dataY), y.some)
  }

  test("ItcSeries.fromLegacy drops the samples at or below 0 nm") {
    val s = series(axis, dataY).get
    assertEquals(s.xAxis.start, 1.0)
    assertEquals(s.xAxis.end, 8.0)
    assertEquals(s.xAxis.count, 8)
    assertEquals(s.xAxis.step, 1.0)
    assertEquals(s.dataY.length, s.xAxis.count)
    assertEquals(s.dataY.head, 3.0)
  }

  test("ItcSeries.fromLegacy keeps every surviving sample at its own wavelength") {
    val s = series(axis, dataY).get
    // Before trimming, 1 nm sat at index 3 holding 3.0; it must still resolve to 3.0
    assertEquals(s.yValueAtWavelength(Wavelength.fromIntNanometers(1).get), 3.0.some)
    assertEquals(s.yValueAtWavelength(Wavelength.fromIntNanometers(8).get), 10.0.some)
    assertEquals(s.xAxis.wavelengthAt(0), Wavelength.fromIntNanometers(1))
  }

  test("ItcSeries.fromLegacy recomputes the y-axis from the surviving samples") {
    val s = series(axis, dataY).get
    assertEquals(s.yAxis.max, 10.0)
    assertEquals(s.yAxis.indexOfMax, 7)
    assertEquals(s.yAxis.min, 3.0)
    assertEquals(s.yAxis.indexOfMin, 0)
    assertEquals(s.wavelengthAtMaxAndMax.map(_._1), Wavelength.fromIntNanometers(8))
  }

  test("ItcSeries.fromLegacy is empty when no sample is above 0 nm") {
    assertEquals(series(ItcXAxis(-5.0, -1.0, 5), NonEmptyList.of(1.0, 2.0, 3.0, 4.0, 5.0)), none)
  }
