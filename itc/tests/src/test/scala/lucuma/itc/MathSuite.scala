// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.math

class MathSuite extends munit.FunSuite:
  test("rounding") {
    assertEquals(roundToSignificantFigures(10.0, 0), 10.0)
    assertEquals(roundToSignificantFigures(10.0, 1), 10.0)
    assertEquals(roundToSignificantFigures(10.0001, 6), 10.0001)
    assertEquals(roundToSignificantFigures(10.0001, 5), 10.0)
    assertEquals(roundToSignificantFigures(10.0005, 5), 10.001)
    assertEquals(roundToSignificantFigures(10.0004, 5), 10.0)
    assertEquals(roundToSignificantFigures(11111.5, 4), 11110.0)
    assertEquals(roundToSignificantFigures(0.123456, 4), 0.1235)
    assertEquals(roundToSignificantFigures(0.0123456, 3), 0.0123)
    assertEquals(roundToSignificantFigures(0.000123456, 3), 0.000123)
  }
