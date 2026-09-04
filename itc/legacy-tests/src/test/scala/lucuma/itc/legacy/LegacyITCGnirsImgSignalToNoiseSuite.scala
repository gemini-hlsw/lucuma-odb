// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

/**
 * Unit test for GNIRS imaging (acquisition) integration time (signal-to-noise) calculation. Mirrors
 * [[LegacyITCGnirsImgExpTimeSuite]] but exercises the integration-time direction (given a desired
 * S/N, the legacy ITC computes exposure time and count).
 */
class LegacyITCGnirsImgSignalToNoiseSuite extends CommonITCLegacySuite:

  override def obs = ItcObservationDetails(
    calculationMethod =
      ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.ImagingIntegrationTime(
        sigma = 100,
        coadds = None,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
    analysisMethod = lsAnalysisMethod
  )

  val gnirs = ObservingMode.ImagingMode.Gnirs(
    filter = GnirsFilter.H2,
    camera = GnirsCamera.ShortBlue,
    readMode = GnirsReadMode.Bright,
    wellDepth = GnirsWellDepth.Shallow,
    coadds = PosInt.unsafeFrom(1),
    portDisposition = PortDisposition.Bottom
  )

  override def instrument = ItcInstrumentDetails(gnirs)

  // The imaging path must actually compute (not just parse): asserts valid results directly,
  // guarding the GnirsParameters centralWavelength unit handling on the imaging branch.
  test("gnirs imaging signal-to-noise base config yields valid results".tag(LegacyITCTest)):
    val result = localItc.calculate(baseParams.asJson.noSpaces)
    assertIOBoolean(result.map(_.fold(_ => false, containsValidResults)))

  test("gnirs imaging filter".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsFilter].all): f =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(filter = f)).asJson.noSpaces

  test("gnirs imaging camera".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsCamera].all): c =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(camera = c)).asJson.noSpaces

  test("gnirs imaging read mode".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsReadMode].all): r =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(readMode = r)).asJson.noSpaces

  test("gnirs imaging well depth".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsWellDepth].all): w =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(wellDepth = w)).asJson.noSpaces

  testConditions("GNIRS imaging integration time", baseParams)

  testSEDs("GNIRS imaging integration time", baseParams)

  testUserDefinedSED("GNIRS imaging integration time", baseParams)

  testBrightnessUnits("GNIRS imaging integration time", baseParams)

  testPowerAndBlackbody("GNIRS imaging integration time", baseParams)
