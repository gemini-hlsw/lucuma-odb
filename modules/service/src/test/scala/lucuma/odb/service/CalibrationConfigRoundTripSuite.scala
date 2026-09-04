// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.arb.ArbCalibrationConfigSubset.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class CalibrationConfigRoundTripSuite extends ScalaCheckSuite:

  private def gmosNorth(input: ObservingModeInput.Create): Option[GmosNConfigs] =
    input.gmosNorthLongSlit.flatMap: ls =>
      val c = ls.common
      (c.explicitXBin, c.explicitYBin, c.explicitAmpReadMode, c.explicitAmpGain, c.explicitRoi)
        .mapN: (x, y, rm, g, r) =>
          GmosNConfigs(ls.grating, ls.filter, ls.fpu, c.centralWavelength, x, y, rm, g, r)

  private def gmosSouth(input: ObservingModeInput.Create): Option[GmosSConfigs] =
    input.gmosSouthLongSlit.flatMap: ls =>
      val c = ls.common
      (c.explicitXBin, c.explicitYBin, c.explicitAmpReadMode, c.explicitAmpGain, c.explicitRoi)
        .mapN: (x, y, rm, g, r) =>
          GmosSConfigs(ls.grating, ls.filter, ls.fpu, c.centralWavelength, x, y, rm, g, r)

  private def gmosNorthIfu(input: ObservingModeInput.Create): Option[GmosNIfuConfigs] =
    input.gmosNorthIfu.flatMap: ifu =>
      val c = ifu.common
      (c.explicitXBin, c.explicitYBin, c.explicitAmpReadMode, c.explicitAmpGain, c.explicitRoi)
        .mapN: (x, y, rm, g, r) =>
          GmosNIfuConfigs(ifu.grating, ifu.filter, ifu.fpu, c.centralWavelength, x, y, rm, g, r)

  private def gmosSouthIfu(input: ObservingModeInput.Create): Option[GmosSIfuConfigs] =
    input.gmosSouthIfu.flatMap: ifu =>
      val c = ifu.common
      (c.explicitXBin, c.explicitYBin, c.explicitAmpReadMode, c.explicitAmpGain, c.explicitRoi)
        .mapN: (x, y, rm, g, r) =>
          GmosSIfuConfigs(ifu.grating, ifu.filter, ifu.fpu, c.centralWavelength, x, y, rm, g, r)

  test("GmosNConfigs round trips through toLongSlitInput"):
    forAll: (c: GmosNConfigs) =>
      assertEquals(gmosNorth(c.toLongSlitInput), c.some)

  test("GmosSConfigs round trips through toLongSlitInput"):
    forAll: (c: GmosSConfigs) =>
      assertEquals(gmosSouth(c.toLongSlitInput), c.some)

  test("GmosNIfuConfigs round trips through toIfuInput"):
    forAll: (c: GmosNIfuConfigs) =>
      assertEquals(gmosNorthIfu(c.toIfuInput), c.some)

  test("GmosSIfuConfigs round trips through toIfuInput"):
    forAll: (c: GmosSIfuConfigs) =>
      assertEquals(gmosSouthIfu(c.toIfuInput), c.some)
