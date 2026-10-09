// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.core.syntax.timespan.*
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.arb.ArbCalibrationConfigSubset.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class SpecPhotoExposureTimeModeSuite extends ScalaCheckSuite:

  private val One: PosInt = PosInt.unsafeFrom(1)

  private def etm(config: CalibrationConfigSubset.Gmos, at: Option[Wavelength]) =
    CalibrationObservations.specPhotoExposureTimeMode(config, at)

  property("long slit uses 120s x 1 at the science wavelength"):
    forAll: (gn: GmosNConfigs, gs: GmosSConfigs, at: Wavelength) =>
      assertEquals(etm(gn, Some(at)), TimeAndCountMode(120.secondTimeSpan, One, at))
      assertEquals(etm(gs, Some(at)), TimeAndCountMode(120.secondTimeSpan, One, at))

  property("IFU uses 300s x 1 at the science wavelength"):
    forAll: (gn: GmosNIfuConfigs, gs: GmosSIfuConfigs, at: Wavelength) =>
      assertEquals(etm(gn, Some(at)), TimeAndCountMode(300.secondTimeSpan, One, at))
      assertEquals(etm(gs, Some(at)), TimeAndCountMode(300.secondTimeSpan, One, at))

  property("falls back to the central wavelength"):
    forAll: (gn: GmosNConfigs, ifu: GmosSIfuConfigs) =>
      assertEquals(etm(gn, None).at, gn.centralWavelength)
      assertEquals(etm(ifu, None).at, ifu.centralWavelength)
