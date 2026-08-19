// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.model

import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.itc.ItcXAxis
import munit.FunSuite

class ItcXAxisSuite extends FunSuite:
  def toWv(i: Int): Wavelength =
    Wavelength.intPicometers.getOption(i).get

  test("ItcXAxis.at") {
    val x = ItcXAxis(1, 5, 9)
    assertEquals(x.at(0), 1.0)
    assertEquals(x.at(1), 1.5)
    assertEquals(x.at(2), 2.0)
    assertEquals(x.at(8), 5.0)
  }

  test("ItcXAxis.wavelengthAt") {
    val x = ItcXAxis(1, 5, 9)
    assertEquals(x.wavelengthAt(0), toWv(1000).some)
    assertEquals(x.wavelengthAt(1), toWv(1500).some)
    assertEquals(x.wavelengthAt(2), toWv(2000).some)
    assertEquals(x.wavelengthAt(8), toWv(5000).some)
  }
  test("ItcXAxis.indexOf") {
    val x = ItcXAxis(1, 5, 9)
    assertEquals(x.indexOf(0.9), none)
    assertEquals(x.indexOf(1.0), 0.some)
    assertEquals(x.indexOf(1.25), 1.some)
    assertEquals(x.indexOf(1.5), 1.some)
    assertEquals(x.indexOf(2.0), 2.some)
    assertEquals(x.indexOf(2.75), 4.some)
    assertEquals(x.indexOf(3.0), 4.some)
    assertEquals(x.indexOf(3.01), 5.some)
    assertEquals(x.indexOf(3.25), 5.some)
    assertEquals(x.indexOf(5.0), 8.some)
    assertEquals(x.indexOf(5.1), none)
  }

  test("ItcXAxis.indexOf(Wavelength)") {
    val x = ItcXAxis(1, 5, 9)
    assertEquals(x.indexOf(toWv(999)), none)
    assertEquals(x.indexOf(toWv(1000)), 0.some)
    assertEquals(x.indexOf(toWv(1250)), 1.some)
    assertEquals(x.indexOf(toWv(1500)), 1.some)
    assertEquals(x.indexOf(toWv(2000)), 2.some)
    assertEquals(x.indexOf(toWv(3000)), 4.some)
    assertEquals(x.indexOf(toWv(5000)), 8.some)
    assertEquals(x.indexOf(toWv(5100)), none)
  }

  test("ItcXAxis.nonPositiveCount") {
    assertEquals(ItcXAxis(1, 5, 9).nonPositiveCount, 0)
    // Samples at -2, -1, 0, 1, ...: the one exactly at 0 nm has no Wavelength either
    assertEquals(ItcXAxis(-2, 8, 11).nonPositiveCount, 3)
    // Every sample below zero
    assertEquals(ItcXAxis(-5, -1, 5).nonPositiveCount, 5)
    // The blue CCD axis GMOS R150 reports at a central wavelength of 540 nm
    assertEquals(ItcXAxis(-65.827, 328.665, 512).nonPositiveCount, 86)
  }

  test("ItcXAxis.drop keeps the spacing and the surviving wavelengths") {
    val x = ItcXAxis(-2, 8, 11)
    val d = x.drop(3)
    assertEquals(d, ItcXAxis(1.0, 8.0, 8))
    assertEquals(d.step, x.step)
    assertEquals(d.at(0), x.at(3))

    // With a fractional step the spacing must survive the shift
    val r = ItcXAxis(-65.827, 328.665, 512)
    assertEqualsDouble(r.drop(86).step, r.step, 1e-9)
    assertEqualsDouble(r.drop(86).at(0), r.at(86), 1e-9)
    assert(r.drop(86).start > 0)
  }
