// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.all.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

/**
 * Unit test for GNIRS exposure time calculation
 */
class LegacyITCGnirsSpecExpTimeSuite extends CommonITCLegacySuite:

  val centralWavelength = Wavelength.decimalMicrometers.getOption(2.2).get
  val wavelengthAt      = Wavelength.decimalMicrometers.getOption(2.1).get

  override def obs = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.SpectroscopyS2N(
      frameCount = 30,
      exposureDuration = 120.seconds,
      wavelengthAt = wavelengthAt,
      coadds = 1.some,
      sourceFraction = 1.0,
      ditherOffset = Angle.fromDoubleArcseconds(5)
    ),
    analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(1)
  )

  val gnirs = ObservingMode.SpectroscopyMode.GnirsSpectroscopy(
    centralWavelength = centralWavelength,
    grating = GnirsGrating.D32,
    filter = GnirsFilter.Order3,
    camera = GnirsCamera.ShortBlue,
    prism = GnirsPrism.Mirror,
    readMode = GnirsReadMode.Bright,
    fpu = GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
    wellDepth = GnirsWellDepth.Shallow,
    coadds = PosInt.unsafeFrom(1),
    portDisposition = PortDisposition.Bottom
  )

  override def instrument = ItcInstrumentDetails(gnirs)

  test("gnirs grating".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsGrating].all): g =>
      localItc.calculate:
        bodyConf(
          sourceDefinition,
          obs,
          gnirs.copy(grating = g, camera = gnirsCameraForGrating(g, gnirs.camera))
        ).asJson.noSpaces

  test("gnirs filter".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsFilter].all): f =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(filter = f)).asJson.noSpaces

  test("gnirs camera".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsCamera].all): c =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(camera = c)).asJson.noSpaces

  test("gnirs prism".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsPrism].all): p =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(prism = p)).asJson.noSpaces

  test("gnirs read mode".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsReadMode].all): r =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(readMode = r)).asJson.noSpaces

  test("gnirs slit width".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsFpuSlit].all): s =>
      localItc.calculate:
        bodyConf(sourceDefinition,
                 obs,
                 gnirs.copy(fpu = GnirsFpu.Spectroscopy.Slit(s))
        ).asJson.noSpaces

  test("gnirs IFU".tag(LegacyITCTest)):
    assertAllValid(
      Enumerated[GnirsFpuIfu].all,
      resultCheck = containsValidResultsWithSNAt
    ): ifu =>
      val ifuMode = gnirs.copy(
        fpu = GnirsFpu.Spectroscopy.Ifu(ifu),
        camera = gnirsCameraForIfu(ifu)
      )
      localItc.calculate:
        bodyConf(sourceDefinition, obs, ifuMode, gnirsIfuAnalysisMethod).asJson.noSpaces

  test("gnirs well depth".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GnirsWellDepth].all): w =>
      localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(wellDepth = w)).asJson.noSpaces

  testConditions("GNIRS spectroscopy S/N", baseParams)

  testSEDs("GNIRS spectroscopy S/N", baseParams)

  testUserDefinedSED("GNIRS spectroscopy S/N", baseParams)

  testBrightnessUnits("GNIRS spectroscopy S/N", baseParams)

  testPowerAndBlackbody("GNIRS spectroscopy S/N", baseParams)

  // Shortcut 10538: a K = 5 A0V star at S/N 1000 through the D32 long slit, for which the ITC
  // folds the exposures into coadds.  The web ITC answered 4 frames of 2 coadds x 2.3 s, and the
  // recipe must report those coadds rather than silently dividing them out of the frame count.
  // Side-looking port, as the web ITC assumes.
  test("gnirs S/N mode reports the coadds it chose (Shortcut 10538)".tag(LegacyITCTest)):
    val veryBrightStar  = ItcSourceDefinition(
      TargetData(
        SourceProfile.Point(
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
            SortedMap(
              Band.K -> BrightnessValue.unsafeFrom(5).withUnit[VegaMagnitude].toMeasureTagged
            )
          )
        ),
        Redshift.Zero
      ),
      Band.K.asLeft
    )
    val storyObs        = ItcObservationDetails(
      calculationMethod =
        ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.SpectroscopyIntegrationTime(
          sigma = 1000.0,
          coadds = none,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0,
          wavelengthAt = Wavelength.decimalNanometers.getOption(2140).get
        ),
      analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(1)
    )
    val storyConditions = ItcObservingConditions(
      iq = BigDecimal(1.0),
      cc = BigDecimal(0.3),
      wv = WaterVapor.Wet,
      sb = SkyBackground.Bright,
      airmass = 2.0
    )
    localItc
      .calculate(
        ItcParameters(
          veryBrightStar,
          storyObs,
          storyConditions,
          ItcTelescopeDetails(wfs = ItcWavefrontSensor.OIWFS,
                              instrumentPort = PortDisposition.Side
          ),
          ItcInstrumentDetails(gnirs.copy(portDisposition = PortDisposition.Side))
        ).asJson.noSpaces
      )
      .map: result =>
        val calc = result.map(_.exposureCalculation.detectors.head)
        assertEquals(calc.map(_.frameCount.value), Right(4))
        assertEquals(calc.map(_.coadds.value), Right(2))
        assertEqualsDouble(calc.map(_.exposureTime).getOrElse(0.0), 2.3, 0.01)
