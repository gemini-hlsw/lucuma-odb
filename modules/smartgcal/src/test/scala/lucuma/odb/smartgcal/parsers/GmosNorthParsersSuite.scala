// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.syntax.bifunctor.*
import cats.syntax.option.*
import cats.syntax.show.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey

final class GmosNorthParsersSuite extends munit.FunSuite {

  test("simple success, key") {

    val definition = "$R400.*,$.*one|.*G.*,$.* 0.50 arcsec,4,1,675 - 1200,1,Low"

    // $.*one|.*G.*  Matches all the filters.  Seems like it could have been just "*".
    val filters = NonEmptyList(none, Enumerated[GmosNorthFilter].all.map(_.some))

    // Both these are 0.50 arcsec
    val fpus    = NonEmptyList.of(GmosNorthFpu.LongSlit_0_50.some, GmosNorthFpu.Ns1.some)

    val expected =
      for {
        f <- filters
        u <- fpus
      } yield TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu](
        GratingConfigKey(
          GmosNorthGrating.R400_G5310,
          GmosGratingOrder.One,
          BoundedInterval.unsafeOpenUpper(
            Wavelength.fromIntNanometers(675).get,
            Wavelength.fromIntNanometers(1200).get
          )
        ).some,
        f,
        u,
        GmosXBinning.Four,
        GmosYBinning.One,
        GmosAmpGain.Low
      )

    assertEquals(
      gmosNorth.fileKey.parseAll(definition).map {
        case Availability.Current(a) => a.tableKeys
        case _                       => sys.error("Not expecting obsolete value here.")
      },
      Right(expected)
    )

  }

  test("No disperser, ignore order and wavelength range") {

    val definition = "Mirror,$.*one|.*G.*,$.* 0.50 arcsec,4,1,675 - 1200,*,Low"

    val filters = NonEmptyList(none, Enumerated[GmosNorthFilter].all.map(_.some))

    // Both these are 0.50 arcsec
    val fpus    = NonEmptyList.of(GmosNorthFpu.LongSlit_0_50.some, GmosNorthFpu.Ns1.some)

    val expected =
      for {
        f <- filters
        u <- fpus
      } yield TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu](
        none,
        f,
        u,
        GmosXBinning.Four,
        GmosYBinning.One,
        GmosAmpGain.Low
      )

    assertEquals(
      gmosNorth.fileKey.parseAll(definition).map {
        case Availability.Current(a) => a.tableKeys
        case _                       => sys.error("Not expecting obsolete value here")
      },
      Right(expected)
    )

  }

  test("simple failure, no matches") {

    val definition = "$.*FooBar.*,$.*one|.*G.*,$.* 0.50 arcsec,4,1,675 - 1200,1,Low"
    assertEquals(
      gmosNorth.fileKey.parseAll(definition).leftMap(_.show),
      Left(
        """$.*FooBar.*,$.*one|.*G.*,$.* 0.50 arcsec,4,1,675 - 1200,1,Low
           |           ^
           |expectation:
           |* context: GMOS North grating, must fail: Pattern '.*FooBar.*' matched nothing, must match one of the strings: {'Mirror', 'B1200_G5301', 'R831_G5302', 'R600_G5304', 'B480_G5309', 'R400_G5310', 'R150_G5308'}""".stripMargin
      )
    )

  }

  test("simple failure, gmos north filter hack") {

    val definition = "$R400.*,foo,$.* 0.50 arcsec,4,1,675 - 1200,1,Low"
    assertEquals(
      gmosNorth.fileKey.parseAll(definition).leftMap(_.show),
      Left(
        """$R400.*,foo,$.* 0.50 arcsec,4,1,675 - 1200,1,Low
           |           ^
           |expectation:
           |* context: GMOS North filter, must fail: Key 'foo' not found, must match one of the strings: {'None', 'g_G0301', 'r_G0303', 'i_G0302', 'z_G0304', 'Z_G0322', 'Y_G0323', 'ri_G0349', 'GG455_G0305', 'OG515_G0306', 'RG610_G0307', 'CaT_G0309', 'Ha_G0310', 'HaC_G0311', 'DS920_G0312', 'SII_G0317', 'OIII_G0318', 'OIIIC_G0319', 'HeII_G0320', 'HeIIC_G0321', 'OVI_G0345', 'OVIC_G0346', 'HartmannA_G0313 + r_G0303', 'HartmannB_G0314 + r_G0303', 'g_G0301 + GG455_G0305', 'g_G0301 + OG515_G0306', 'r_G0303 + RG610_G0307', 'i_G0302 + CaT_G0309', 'z_G0304 + CaT_G0309'}""".stripMargin
      )
    )
  }

}
