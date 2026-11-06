// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.util.TimeSpan
import lucuma.odb.smartgcal.data.Gnirs.TableKey
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig

final class GnirsParsersSuite extends munit.FunSuite:

  private def range(loNm: Int, hiNm: Int): BoundedInterval[Wavelength] =
    BoundedInterval.unsafeOpenUpper(
      Wavelength.fromIntNanometers(loNm).get,
      Wavelength.fromIntNanometers(hiNm).get
    )

  test("imaging row: no grating or prism, acquisition FPU, keyed by filter wavelength"):
    val definition = "Imaging,\"0.15\"\"/pix\",*,*,1.17 - 1.37,*,Shallow,IR grey body - high,Open,ND4.0,IR,8,1.5,1,Night"

    val expectedKey = TableKey(
      GnirsPixelScale.PixelScale_0_15,
      none,
      none,
      range(1170, 1370),
      GnirsFpu.Other(GnirsFpuOther.Acquisition),
      GnirsWellDepth.Shallow
    )

    val expectedValue = SmartGcalValue(
      Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.IrGreyBodyHigh), GcalFilter.Nd40, GcalDiffuser.Ir, GcalShutter.Open),
      GcalBaselineType.Night,
      PosInt.unsafeFrom(8),
      LegacyInstrumentConfig(TimeSpan.unsafeFromMicroseconds(1_500_000L), PosInt.unsafeFrom(1))
    )

    assertEquals(
      gnirs.fileEntry.parseAll(definition).map(e => (e.key.tableKeys, e.value)),
      Right((NonEmptyList.one(expectedKey), expectedValue))
    )

  test("imaging row: the spectroscopy columns must be wildcards"):
    val definition = "Imaging,\"0.15\"\"/pix\",32 l/mm grating,*,1.17 - 1.37,*,Shallow,IR grey body - high,Open,ND4.0,IR,8,1.5,1,Night"
    assert(gnirs.fileEntry.parseAll(definition).isLeft)

  test("spectroscopy row: grating, prism and slit fan out into the key"):
    val definition = "Spectroscopy,\"0.05\"\"/pix\",10 l/mm grating,LXD,0.9 - 2.56,0.10 arcsec,Shallow,IR grey body - high,Open,none,IR,5,3,1,Night"

    val expectedKey = TableKey(
      GnirsPixelScale.PixelScale_0_05,
      GnirsGrating.D10.some,
      GnirsPrism.Lxd.some,
      range(900, 2560),
      GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_10),
      GnirsWellDepth.Shallow
    )

    assertEquals(
      gnirs.fileEntry.parseAll(definition).map(_.key.tableKeys),
      Right(NonEmptyList.one(expectedKey))
    )
