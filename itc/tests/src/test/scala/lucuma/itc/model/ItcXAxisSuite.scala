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
