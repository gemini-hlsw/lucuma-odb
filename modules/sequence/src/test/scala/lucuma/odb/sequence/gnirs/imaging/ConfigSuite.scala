// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.imaging

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
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

  test("coaddsFor picks up each filter's own value"):
    val c = config(NonEmptyList.of(j, order4))
    assertEquals(c.coaddsFor(GnirsFilter.J), PosInt.unsafeFrom(2))
    assertEquals(c.coaddsFor(GnirsFilter.Order4), PosInt.unsafeFrom(5))

  test("coaddsFor defaults to 1 for a filter not in the configuration"):
    assertEquals(config(NonEmptyList.one(j)).coaddsFor(GnirsFilter.K), PosInt.unsafeFrom(1))

  test("changing a filter's coadds changes the hash"):
    val a = config(NonEmptyList.of(j, order4))
    val b = config(NonEmptyList.of(j.copy(coadds = PosInt.unsafeFrom(3)), order4))
    assert(!java.util.Arrays.equals(a.hashBytes, b.hashBytes))

  test("dropping a filter changes the hash"):
    val a = config(NonEmptyList.of(j, order4))
    val b = config(NonEmptyList.one(j))
    assert(!java.util.Arrays.equals(a.hashBytes, b.hashBytes))
