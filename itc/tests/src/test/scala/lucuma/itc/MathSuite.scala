// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.math

class MathSuite extends munit.FunSuite:
  test("rounding") {
    assertEquals(10.0, roundToSignificantFigures(10.0, 0))
    assertEquals(10.0, roundToSignificantFigures(10.0, 1))
    assertEquals(10.0001, roundToSignificantFigures(10.0001, 6))
    assertEquals(10.0, roundToSignificantFigures(10.0001, 5))
    assertEquals(10.001, roundToSignificantFigures(10.0005, 5))
    assertEquals(10.0, roundToSignificantFigures(10.0004, 5))
    assertEquals(11110.0, roundToSignificantFigures(11111.5, 4))
  }
