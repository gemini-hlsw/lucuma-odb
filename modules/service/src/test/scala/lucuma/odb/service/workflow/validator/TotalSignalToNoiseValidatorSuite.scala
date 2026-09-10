// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.data.NonEmptyList
import lucuma.core.math.Wavelength
import munit.FunSuite

class TotalSignalToNoiseValidatorSuite extends FunSuite:

  private def ws(nm: Int*): NonEmptyList[Wavelength] =
    NonEmptyList.fromListUnsafe(nm.toList.map(n => Wavelength.unsafeFromIntPicometers(n * 1000)))

  private def labels(nm: Int*): List[String] =
    TotalSignalToNoiseValidator.gnirsSpectroscopyLabels(ws(nm*)).toList

  test("a single wavelength is named without an ordinal"):
    assertEquals(labels(2200), List("2200.000 nm"))

  test("distinct wavelengths are named without ordinals"):
    assertEquals(labels(1000, 2200), List("1000.000 nm", "2200.000 nm"))

  test("a repeated wavelength carries a 1-based occurrence ordinal"):
    assertEquals(labels(2200, 2200), List("2200.000 nm #1", "2200.000 nm #2"))

  test("only the repeated wavelength is numbered, and it is numbered in list order"):
    // Non-adjacent duplicates: the ordinals follow the position in the list, and the
    // wavelength that occurs once keeps its bare label.
    assertEquals(
      labels(1000, 2200, 1000),
      List("1000.000 nm #1", "2200.000 nm", "1000.000 nm #2")
    )

  test("two wavelengths may repeat independently"):
    assertEquals(
      labels(1000, 2200, 2200, 1000),
      List("1000.000 nm #1", "2200.000 nm #1", "2200.000 nm #2", "1000.000 nm #2")
    )
