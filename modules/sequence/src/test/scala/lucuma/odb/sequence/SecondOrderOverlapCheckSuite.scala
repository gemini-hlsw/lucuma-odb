// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.all.*
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.arb.ArbStep.given
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.model.sequence.gmos.SecondOrderOverlap
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig.given
import munit.FunSuite
import org.scalacheck.Arbitrary

import SecondOrderOverlapCheck.*

class SecondOrderOverlapCheckSuite extends FunSuite:

  private def sample[A](using a: Arbitrary[A]): A =
    Iterator.continually(a.arbitrary.sample).flatten.next()

  private def nm(n: Int): Wavelength =
    Wavelength.unsafeFromIntPicometers(n * 1000)

  private def north(
    grating:    Option[GmosNorthGrating],
    filter:     Option[GmosNorthFilter],
    λ:          Int,
    stepConfig: StepConfig = StepConfig.Science
  ): Step[DynamicConfig.GmosNorth] =
    val d = sample[DynamicConfig.GmosNorth].copy(
      gratingConfig = grating.map(g => GmosGratingConfig.North(g, GmosGratingOrder.One, nm(λ))),
      filter        = filter
    )
    sample[Step[DynamicConfig.GmosNorth]].copy(instrumentConfig = d, stepConfig = stepConfig)

  private def check(s: Step[DynamicConfig.GmosNorth]): Option[SecondOrderOverlap] =
    SecondOrderOverlapCheck[DynamicConfig.GmosNorth].check(s)

  test("the onset is twice the filter cut-on, or twice the no-filter limit"):
    assertEquals(onset(None), nm(720).some)
    assertEquals(onset(GmosNorthFilter.GG455.width.some), nm(920).some)

  test("no filter, coverage reaching past 720 nm"):
    // R400 at 700 nm covers 464 - 936 nm
    assertEquals(check(north(GmosNorthGrating.R400_G5310.some, None, 700)), SecondOrderOverlap(nm(720), nm(700)).some)

  test("no filter, coverage short of 720 nm"):
    // B1200 at 450 nm covers 368 - 532 nm
    assertEquals(check(north(GmosNorthGrating.B1200_G5301.some, None, 450)), None)

  test("long pass filter, coverage reaching past twice its cut-on"):
    // R400 at 800 nm covers 564 - 1036 nm, GG455 overlap starts at 920 nm
    assertEquals(check(north(GmosNorthGrating.R400_G5310.some, GmosNorthFilter.GG455.some, 800)), SecondOrderOverlap(nm(920), nm(800)).some)

  test("long pass filter, coverage short of twice its cut-on"):
    // R831 at 750 nm covers 632.5 - 867.5 nm, OG515 overlap starts at 1040 nm
    assertEquals(check(north(GmosNorthGrating.R831_G5302.some, GmosNorthFilter.OG515.some, 750)), None)

  test("band pass filter ending before the onset passes no first order light there"):
    // R150 at 500 nm reaches 1109.5 nm, but g ends at 552 nm, before the 796 nm onset
    assertEquals(check(north(GmosNorthGrating.R150_G5308.some, GmosNorthFilter.GPrime.some, 500)), None)

  test("the coverage edge"):
    // R400 coverage is 472 nm, so it reaches past 720 nm for central wavelengths above 484 nm
    assertEquals(check(north(GmosNorthGrating.R400_G5310.some, None, 484)), None)
    assertEquals(check(north(GmosNorthGrating.R400_G5310.some, None, 485)), SecondOrderOverlap(nm(720), nm(485)).some)

  test("steps without a grating are not checked"):
    assertEquals(check(north(None, None, 700)), None)

  test("only science steps are checked"):
    assertEquals(check(north(GmosNorthGrating.R400_G5310.some, None, 700, StepConfig.Bias)), None)
    assertEquals(check(north(GmosNorthGrating.R400_G5310.some, None, 700, StepConfig.Dark)), None)

  test("GMOS South"):
    // R400 at 700 nm covers 469 - 931 nm
    val d = sample[DynamicConfig.GmosSouth].copy(
      gratingConfig = GmosGratingConfig.South(GmosSouthGrating.R400_G5325, GmosGratingOrder.One, nm(700)).some,
      filter        = None
    )
    val s = sample[Step[DynamicConfig.GmosSouth]].copy(instrumentConfig = d, stepConfig = StepConfig.Science)
    assertEquals(SecondOrderOverlapCheck[DynamicConfig.GmosSouth].check(s), SecondOrderOverlap(nm(720), nm(700)).some)
    assertEquals(
      SecondOrderOverlapCheck[DynamicConfig.GmosSouth].check(s.copy(instrumentConfig = d.copy(filter = GmosSouthFilter.OG515.some))),
      None
    )
