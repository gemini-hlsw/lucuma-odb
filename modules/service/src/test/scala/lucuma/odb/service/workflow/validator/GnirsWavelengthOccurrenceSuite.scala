// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.odb.sequence.gnirs.wavelengthOccurrences
import munit.FunSuite

/**
 * The occurrence rule itself, plus the two message families that read it.  A GNIRS
 * observation may repeat a central wavelength, each occurrence being an independent
 * configuration, so a message that names one has to say which.  Every case here that
 * involves a repeat asserts a `#n` the previous bare-wavelength messages could not
 * produce; every case without one asserts the message is unchanged.
 */
class GnirsWavelengthOccurrenceSuite extends FunSuite:

  private def ws(nm: Int*): List[Wavelength] =
    nm.toList.map(n => Wavelength.unsafeFromIntPicometers(n * 1000))

  private def occ(nm: Int*): List[Option[Int]] =
    wavelengthOccurrences(ws(nm*))

  test("a wavelength that occurs once has no ordinal"):
    assertEquals(occ(2200), List(None))
    assertEquals(occ(1000, 2200), List(None, None))

  test("a repeated wavelength is numbered in list order"):
    assertEquals(occ(2200, 2200), List(Some(1), Some(2)))

  test("only the repeated wavelength is numbered"):
    assertEquals(occ(1000, 2200, 1000), List(Some(1), None, Some(2)))

  test("two wavelengths may repeat independently"):
    assertEquals(occ(1000, 2200, 2200, 1000), List(Some(1), Some(1), Some(2), Some(2)))

  test("three occurrences run 1, 2, 3"):
    assertEquals(occ(2200, 2200, 2200), List(Some(1), Some(2), Some(3)))

  // The low signal-to-noise warning names wavelengths in nm.
  private def labels(nm: Int*): List[String] =
    TotalSignalToNoiseValidator
      .gnirsSpectroscopyLabels(cats.data.NonEmptyList.fromListUnsafe(ws(nm*)))
      .toList

  test("signal-to-noise labels are unchanged when the wavelength occurs once"):
    assertEquals(labels(1000, 2200), List("1000.000 nm", "2200.000 nm"))

  test("signal-to-noise labels carry the ordinal when the wavelength repeats"):
    assertEquals(labels(2200, 2200), List("2200.000 nm #1", "2200.000 nm #2"))
    assertEquals(
      labels(1000, 2200, 1000),
      List("1000.000 nm #1", "2200.000 nm", "1000.000 nm #2")
    )

  // The configuration and exposure checks name wavelengths in µm.
  test("configuration check messages are unchanged when the wavelength occurs once"):
    assertEquals(
      GnirsSpectroscopyValidator.filterMismatch(GnirsFilter.Order4, ws(2200).head),
      "Filter H does not cover the central wavelength 2.200 µm."
    )

  test("configuration check messages carry the ordinal when the wavelength repeats"):
    assertEquals(
      GnirsSpectroscopyValidator.filterMismatch(GnirsFilter.Order4, ws(2200).head, Some(2)),
      "Filter H does not cover the central wavelength 2.200 µm #2."
    )

  test("exposure check messages carry the ordinal when the wavelength repeats"):
    assert(
      GnirsSpectroscopyValidator
        .exposureTooShort(GnirsReadMode.VeryFaint, ws(2200).head, Some(1))
        .endsWith("(at 2.200 µm #1).")
    )
    assert(
      GnirsSpectroscopyValidator
        .exposureUnusuallyLong(GnirsReadMode.VeryBright, 1.secTimeSpan, ws(2200).head, Some(2))
        .endsWith("(at 2.200 µm #2).")
    )
