// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyMap
import cats.implicits.*
import coulomb.Quantity
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.Main.ReverseClassLoader
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData
import munit.FunSuite
import munit.Tag

import java.io.File
import java.io.FileFilter
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

object LegacyITCTest extends Tag("LegacyItcTest")

/**
 * This is a common trait for tests of the legacy ITC code
 */
trait CommonITCLegacySuite extends FunSuite:
  // Default timeout for all legacy ITC tests
  override def munitTimeout: Duration = 5.minute

  // Common validation functions
  def containsValidResults(r: IntegrationTimeRemoteResult): Boolean =
    r.exposureCalculation.selectedIndex < r.exposureCalculation.exposures.length &&
      r.exposureCalculation.exposures.forall(e => e.exposureTime >= 0 && e.exposureCount.value >= 0)

  def allowedErrors(err: List[String]) =
    err.exists(_.contains("Invalid S/N")) || err.exists(_.contains("do not overlap")) ||
      err.exists(_.contains("Unsupported configuration")) ||
      err.exists(_.contains("Unsupported calculation method")) ||
      err.exists(_.matches("Configuration would require \\d* exposures")) ||
      err.exists(_.contains("target is too bright")) ||
      err.exists(_.contains("Signal = 0")) ||
      err.exists(_.contains("Redshifted SED")) ||
      err.exists(_.contains("Wavelength"))

  def allowedErrorsWithLargeSN(err: List[String]) =
    err.exists(_.contains("Invalid S/N")) || err.exists(_.contains("do not overlap")) ||
      err.exists(_.contains("Unsupported configuration")) ||
      err.exists(_.contains("Unsupported calculation method")) ||
      err.exists(_.matches("Configuration would require \\d* exposures")) ||
      err.exists(_.contains("target is too bright")) ||
      err.exists(_.contains("Invalid SignalToNoise value"))

  // Initialize the local ITC
  lazy val localItc = {
    val jarFiles =
      new File("itc/service/ocslib")
        .getAbsoluteFile()
        .listFiles(new FileFilter() {
          override def accept(file: File): Boolean =
            file.getName().endsWith(".jar")
        })
    LocalItc(
      new ReverseClassLoader(jarFiles.map(_.toURI.toURL), ClassLoader.getSystemClassLoader())
    )
  }

  // Define the source profile for this test suite
  def sourceDefinition = ItcSourceDefinition(
    TargetData(
      SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
          SortedMap(
            Band.R -> BrightnessValue
              .unsafeFrom(12)
              .withUnit[VegaMagnitude]
              .toMeasureTagged
          )
        )
      ),
      Redshift(0.03)
    ),
    Band.R.asLeft
  )

  // Common analysis methods - these will be used in tests
  def lsAnalysisMethod  = ItcObservationDetails.AnalysisMethod.Aperture.Auto(5)
  def ifuAnalysisMethod =
    ItcObservationDetails.AnalysisMethod.Ifu.Single(skyFibres = 250, offset = 5.0)

  // Common telescope details - this will be used in tests
  def telescope = ItcTelescopeDetails(
    wfs = ItcWavefrontSensor.OIWFS
  )

  // Common observing conditions - this will be used in tests
  def defaultConditions = ItcObservingConditions(
    ImageQuality.Preset.PointEight.toImageQuality.toArcSeconds,
    CloudExtinction.Preset.OnePointZero.toCloudExtinction.toVegaMagnitude,
    WaterVapor.Median,
    SkyBackground.Bright,
    1
  )

  def obs: ItcObservationDetails
  def instrument: ItcInstrumentDetails

  private def baseParamsFn(obs: ItcObservationDetails, instrument: ItcInstrumentDetails) =
    ItcParameters(
      sourceDefinition,
      obs,
      defaultConditions,
      telescope,
      instrument
    )

  def baseParams = baseParamsFn(obs, instrument)

  // Common builder methods for test parameters
  def bodyCond(
    sourceDefinition: ItcSourceDefinition,
    obs:              ItcObservationDetails,
    instrument:       ItcInstrumentDetails,
    conditions:       ItcObservingConditions
  ): ItcParameters =
    ItcParameters(
      sourceDefinition,
      obs,
      conditions,
      telescope,
      instrument
    )

  def bodyConf(
    sourceDefinition: ItcSourceDefinition,
    obs:              ItcObservationDetails,
    mode:             ObservingMode,
    analysis:         ItcObservationDetails.AnalysisMethod = lsAnalysisMethod
  ): ItcParameters =
    ItcParameters(
      sourceDefinition,
      obs.copy(analysisMethod = analysis),
      ItcObservingConditions(
        ImageQuality.Preset.PointEight.toImageQuality.toArcSeconds,
        CloudExtinction.Preset.OnePointZero.toCloudExtinction.toVegaMagnitude,
        WaterVapor.Median,
        SkyBackground.Dark,
        2
      ),
      telescope,
      ItcInstrumentDetails(mode)
    )

  def bodySED(
    sourceDefinition: ItcSourceDefinition,
    obs:              ItcObservationDetails,
    conditions:       ItcObservingConditions,
    instrument:       ItcInstrumentDetails,
    sed:              UnnormalizedSED
  ): ItcParameters =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.unnormalizedSED
            .modifyOption(_ => sed.some)(sourceDefinition.sourceProfile)
            .getOrElse(sourceDefinition.sourceProfile)
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  def bodyIntMagUnits(
    sourceDefinition: ItcSourceDefinition,
    obs:              ItcObservationDetails,
    conditions:       ItcObservingConditions,
    instrument:       ItcInstrumentDetails,
    brightness:       BrightnessMeasure[Integrated]
  ): ItcParameters =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile
            .integratedBrightnessIn(Band.R)
            .replace(brightness)(sourceDefinition.sourceProfile)
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  def bodySurfaceMagUnits(
    sourceDefinition: ItcSourceDefinition,
    obs:              ItcObservationDetails,
    conditions:       ItcObservingConditions,
    instrument:       ItcInstrumentDetails,
    brightness:       BrightnessMeasure[Surface]
  ): ItcParameters =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.Uniform(
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
              SortedMap(Band.R -> brightness)
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  def bodyIntGaussianMagUnits(
    sourceDefinition: ItcSourceDefinition,
    obs:              ItcObservationDetails,
    conditions:       ItcObservingConditions,
    instrument:       ItcInstrumentDetails,
    brightness:       BrightnessMeasure[Integrated],
    size:             Double = 10.0
  ): ItcParameters =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.Gaussian(
            Angle.fromDoubleArcseconds(size),
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
              SortedMap(Band.R -> brightness)
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  def bodyPowerLaw(
    sourceDefinition: ItcSourceDefinition,
    obs:              ItcObservationDetails,
    conditions:       ItcObservingConditions,
    instrument:       ItcInstrumentDetails,
    index:            Int
  ): ItcParameters =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.Gaussian(
            Angle.fromDoubleArcseconds(10),
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.PowerLaw(index).some,
              SortedMap(
                Band.R ->
                  BrightnessValue
                    .unsafeFrom(5)
                    .withUnit[VegaMagnitude]
                    .toMeasureTagged
              )
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  def bodyBlackBody(
    sourceDefinition: ItcSourceDefinition,
    obs:              ItcObservationDetails,
    conditions:       ItcObservingConditions,
    instrument:       ItcInstrumentDetails,
    temperature:      PosInt
  ): ItcParameters =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.Gaussian(
            Angle.fromDoubleArcseconds(10),
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.BlackBody(temperature.withUnit[Kelvin]).some,
              SortedMap(
                Band.R ->
                  BrightnessValue
                    .unsafeFrom(5)
                    .withUnit[VegaMagnitude]
                    .toMeasureTagged
              )
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  // Common test implementations
  def testConditions(name: String, params: ItcParameters): Unit =
    test(s"$name - image quality".tag(LegacyITCTest)):
      Enumerated[ImageQuality.Preset].all.foreach: iq =>
        val result = localItc
          .calculateIntegrationTime(
            params
              .copy(conditions = params.conditions.copy(iq = iq.toImageQuality.toArcSeconds))
              .asJson
              .noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - cloud extinction".tag(LegacyITCTest)):
      Enumerated[CloudExtinction.Preset].all.foreach: ce =>
        val result = localItc
          .calculateIntegrationTime(
            params
              .copy(conditions = params.conditions.copy(cc = ce.toCloudExtinction.toVegaMagnitude))
              .asJson
              .noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - water vapor".tag(LegacyITCTest)):
      Enumerated[WaterVapor].all.foreach: wv =>
        val result = localItc
          .calculateIntegrationTime(
            params.copy(conditions = params.conditions.copy(wv = wv)).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - sky background".tag(LegacyITCTest)):
      Enumerated[SkyBackground].all.foreach: sb =>
        val result = localItc
          .calculateIntegrationTime(
            params.copy(conditions = params.conditions.copy(sb = sb)).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

  def testSEDs(
    name:        String,
    baseParams:  ItcParameters,
    runStellar:  Boolean = true,
    runCoolStar: Boolean = true
  ): Unit =
    test(s"$name - stellar library spectrum".tag(LegacyITCTest)):
      assume(runStellar, "Skip stellar library spectrum test")
      Enumerated[StellarLibrarySpectrum].all.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.StellarLibrary(f)
            ).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - cool star".tag(LegacyITCTest)):
      assume(runCoolStar, "Skip cool star test")
      Enumerated[CoolStarTemperature].all.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.CoolStarModel(f)
            ).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - galaxy spectrum".tag(LegacyITCTest)):
      Enumerated[GalaxySpectrum].all.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.Galaxy(f)
            ).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - planet spectrum".tag(LegacyITCTest)):
      Enumerated[PlanetSpectrum].all.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.Planet(f)
            ).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - quasar spectrum".tag(LegacyITCTest)):
      Enumerated[QuasarSpectrum].all.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.Quasar(f)
            ).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - hii region spectrum".tag(LegacyITCTest)):
      Enumerated[HIIRegionSpectrum].all.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.HIIRegion(f)
            ).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

    test(s"$name - planetary nebula spectrum".tag(LegacyITCTest)):
      Enumerated[PlanetaryNebulaSpectrum].all.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.PlanetaryNebula(f)
            ).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))

  def testUserDefinedSED(name: String, baseParams: ItcParameters): Unit =
    test(s"$name - user defined SED".tag(LegacyITCTest)):
      val userDefinedFluxDensities = NonEmptyMap.of(
        Wavelength.decimalNanometers.getOption(300).get -> BigDecimal(0.5),
        Wavelength.decimalNanometers.getOption(500).get -> BigDecimal(1.0),
        Wavelength.decimalNanometers.getOption(600).get -> BigDecimal(0.0),
        Wavelength.decimalNanometers.getOption(700).get -> BigDecimal(-0.1)
      )

      val result = localItc
        .calculateIntegrationTime(
          bodySED(
            baseParams.source,
            baseParams.observation,
            baseParams.conditions,
            baseParams.instrument,
            UnnormalizedSED.UserDefined(userDefinedFluxDensities)
          ).asJson.noSpaces
        )

      assert(result.fold(allowedErrors, containsValidResults))

  def testBrightnessUnits(
    name:       String,
    baseParams: ItcParameters,
    errorCheck: List[String] => Boolean = allowedErrors
  ): Unit =
    test(s"$name - brightness integrated units".tag(LegacyITCTest)):
      Brightness.Integrated.all.toList.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodyIntMagUnits(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
            ).asJson.noSpaces
          )
        assert(result.fold(errorCheck, containsValidResults))

    test(s"$name - surface units".tag(LegacyITCTest)):
      Brightness.Surface.all.toList.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodySurfaceMagUnits(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
            ).asJson.noSpaces
          )
        assert(result.fold(errorCheck, containsValidResults))

    test(s"$name - gaussian units".tag(LegacyITCTest)):
      Brightness.Integrated.all.toList.foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodyIntGaussianMagUnits(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
            ).asJson.noSpaces
          )
        assert(result.fold(errorCheck, containsValidResults))

  def testPowerAndBlackbody(name: String, baseParams: ItcParameters): Unit =
    test(s"$name - power law".tag(LegacyITCTest)):
      List(-10, 0, 10).foreach: f =>
        val result = localItc
          .calculateIntegrationTime(
            bodyPowerLaw(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              f
            ).asJson.noSpaces
          )
        assert(result.fold(allowedErrors, containsValidResults))
