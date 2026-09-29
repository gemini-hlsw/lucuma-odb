// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.imaging

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.gnirs.AcquisitionConfig
import lucuma.odb.sequence.imaging.Variant
import munit.FunSuite

class ConfigSuite extends FunSuite:

  private def etm(seconds: Double): ExposureTimeMode =
    ExposureTimeMode.TimeAndCountMode(
      TimeSpan.FromSeconds.unsafeGet(BigDecimal(seconds)),
      PosInt.unsafeFrom(3),
      Wavelength.decimalNanometers.unsafeGet(1250.0)
    )

  private def config(filters: NonEmptyList[Filter]): Config =
    Config(
      variant           = Variant.Interleaved.Default,
      filters           = filters,
      camera            = GnirsCamera.ShortBlue,
      explicitReadMode  = None,
      defaultWellDepth  = GnirsWellDepth.Shallow,
      explicitWellDepth = None,
      acquisition       = AcquisitionConfig(None, None, etm(10.0), true, PosInt.unsafeFrom(1))
    )

  private val j      = Filter(GnirsFilter.J, etm(10.0), PosInt.unsafeFrom(2))
  private val order4 = Filter(GnirsFilter.Order4, etm(25.0), PosInt.unsafeFrom(5))

  // An ITC result whose coadds differ from every configured value, so the tests
  // can tell which side won.
  private val itcTime: IntegrationTime =
    IntegrationTime(TimeSpan.FromSeconds.unsafeGet(BigDecimal(4.0)), PosInt.unsafeFrom(6), PosInt.unsafeFrom(4))

  test("coaddsFor picks up each filter's own value in time-and-count mode"):
    val c = config(NonEmptyList.of(j, order4))
    assertEquals(c.coaddsFor(GnirsFilter.J, itcTime), PosInt.unsafeFrom(2))
    assertEquals(c.coaddsFor(GnirsFilter.Order4, itcTime), PosInt.unsafeFrom(5))

  test("coaddsFor takes the ITC's coadds in signal-to-noise mode"):
    val sn = ExposureTimeMode.SignalToNoiseMode(
      SignalToNoise.unsafeFromBigDecimalExact(100),
      Wavelength.decimalNanometers.unsafeGet(1250.0)
    )
    val c  = config(NonEmptyList.of(j.copy(exposureTimeMode = sn), order4))
    assertEquals(c.coaddsFor(GnirsFilter.J, itcTime), PosInt.unsafeFrom(4))
    assertEquals(c.coaddsFor(GnirsFilter.Order4, itcTime), PosInt.unsafeFrom(5))

  test("coaddsFor defaults to 1 for a filter not in the configuration"):
    assertEquals(config(NonEmptyList.one(j)).coaddsFor(GnirsFilter.K, itcTime), PosInt.unsafeFrom(1))

  test("changing a filter's coadds changes the hash"):
    val a = config(NonEmptyList.of(j, order4))
    val b = config(NonEmptyList.of(j.copy(coadds = PosInt.unsafeFrom(3)), order4))
    assert(!java.util.Arrays.equals(a.hashBytes, b.hashBytes))

  test("dropping a filter changes the hash"):
    val a = config(NonEmptyList.of(j, order4))
    val b = config(NonEmptyList.one(j))
    assert(!java.util.Arrays.equals(a.hashBytes, b.hashBytes))
