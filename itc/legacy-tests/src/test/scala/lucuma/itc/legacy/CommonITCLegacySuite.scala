// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyMap
import cats.effect.IO
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
import munit.Location
import munit.Tag

import java.io.File
import java.io.FileFilter
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

object LegacyITCTest extends Tag("LegacyItcTest")

import munit.CatsEffectSuite

/**
 * This is a common trait for tests of the legacy ITC code
 */
trait CommonITCLegacySuite extends CatsEffectSuite:

  // Each of these tests makes one real (and slow) call into the legacy ITC per enum value, and
  // some of those enums are large — StellarLibrarySpectrum alone has 158. The default 30s is not
  // enough now that the assertions actually run.
  override def munitIOTimeout: Duration = 15.minutes

  // Common validation functions
  def containsValidResults(r: IntegrationTimeRemoteResult): Boolean =
    r.exposureCalculation.selectedIndex < r.exposureCalculation.exposures.length &&
      r.exposureCalculation.exposures.forall(e => e.exposureTime >= 0 && e.exposureCount.value >= 0)

  // For modes that must also report the S/N at the requested wavelength. Not every mode does,
  // so this is opt-in rather than being folded into `containsValidResults`.
  def containsValidResultsWithSNAt(r: IntegrationTimeRemoteResult): Boolean =
    containsValidResults(r) && r.signalToNoiseAt.isDefined

  /**
   * Messages the legacy ITC produces when a configuration simply is not calculable, rather than
   * because something is wrong.
   *
   * The wording is not consistent across instruments: REL-4806 rewrote the messages in GmosRecipe
   * but left GnirsRecipe and Flamingos2Recipe on the older phrasing, so both forms are listed here.
   */
  private val notCalculablePatterns: List[String] = List(
    "do not overlap",
    "Unsupported configuration",
    "Unsupported calculation method",
    "target is too bright",
    // GNIRS / Flamingos2 wording
    "Configuration would require",
    // GMOS wording, since REL-4806
    "Insufficient signal at",
    "exposures required for this configuration",
    // No read mode satisfies the configuration, e.g. GNIRS imaging with the Order1 / PAH filters
    "Could not find best read mode"
  )

  // "Signal = 0" is GNIRS/Flamingos2; "Signal is <= 0" is the shared ExposureTimeCalculator.
  private val noSignalPatterns: List[String] = List("Signal = 0", "Signal is <= 0")

  def allowedErrors(err: List[String]): Boolean =
    val patterns = notCalculablePatterns ++ noSignalPatterns ++ List("Redshifted SED", "Wavelength")
    err.exists(e => patterns.exists(e.contains))

  def allowedErrorsWithLargeSN(err: List[String]): Boolean =
    val patterns = notCalculablePatterns :+ "Invalid SignalToNoise value"
    err.exists(e => patterns.exists(e.contains))

  /**
   * Runs `run` for every value and fails listing every value whose result was not acceptable, each
   * paired with why it was rejected.
   *
   * Always iterate with this rather than `foreach`: `assertIOBoolean` returns an `IO`, which
   * `foreach` discards, silently turning the assertion into a no-op.
   */
  def assertAllValid[A](
    values:      List[A],
    errorCheck:  List[String] => Boolean = allowedErrors,
    resultCheck: IntegrationTimeRemoteResult => Boolean = containsValidResults
  )(run: A => IO[Either[List[String], IntegrationTimeRemoteResult]])(using Location): IO[Unit] =
    values
      .traverse: a =>
        run(a).map:
          case Left(errs) if !errorCheck(errs) =>
            (a -> s"disallowed error: ${errs.mkString("; ")}").some
          case Right(r) if !resultCheck(r)     => (a -> s"unacceptable result: $r").some
          case _                               => none
      .map(_.flattenOption)
      .map(assertEquals(_, List.empty[(A, String)]))

  // Initialize the local ITC
  lazy val localItc = {
    val jarFiles =
      new File("itc/service/ocslib")
        .getAbsoluteFile()
        .listFiles(new FileFilter() {
          override def accept(file: File): Boolean =
            file.getName().endsWith(".jar")
        })
    LocalItc[IO](
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

  // The production GNIRS IFU analysis method: "sum of 2x2 elements at the center"
  // with a single sky fibre (see lucuma.itc.service.ObservingMode).
  def gnirsIfuAnalysisMethod =
    ItcObservationDetails.AnalysisMethod.Ifu.Summed(
      skyFibres = 1,
      numX = 2,
      numY = 2,
      centerX = 0.0,
      centerY = 0.0
    )

  // The OCS GNIRS recipe requires a specific camera per IFU resolution: LR-IFU on the
  // 0.15"/pix (Short) camera, HR-IFU on the 0.05"/pix (Long) camera.
  def gnirsCameraForIfu(ifu: GnirsFpuIfu): GnirsCamera =
    ifu match
      case GnirsFpuIfu.LowResolution  => GnirsCamera.ShortBlue
      case GnirsFpuIfu.HighResolution => GnirsCamera.LongBlue

  // The 10 l/mm grating is only usable with a long camera (Gnirs.java rejects it on the
  // 0.15"/pix short camera), so pair it up rather than leaving it untested.
  def gnirsCameraForGrating(grating: GnirsGrating, default: GnirsCamera): GnirsCamera =
    grating match
      case GnirsGrating.D10 => GnirsCamera.LongBlue
      case _                => default

  // Common telescope details - this will be used in tests
  def telescope = ItcTelescopeDetails(
    wfs = ItcWavefrontSensor.OIWFS,
    instrumentPort = instrument.mode.portDisposition
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
      assertAllValid(Enumerated[ImageQuality.Preset].all): iq =>
        localItc
          .calculate(
            params
              .copy(conditions = params.conditions.copy(iq = iq.toImageQuality.toArcSeconds))
              .asJson
              .noSpaces
          )

    test(s"$name - cloud extinction".tag(LegacyITCTest)):
      assertAllValid(Enumerated[CloudExtinction.Preset].all): ce =>
        localItc
          .calculate(
            params
              .copy(conditions = params.conditions.copy(cc = ce.toCloudExtinction.toVegaMagnitude))
              .asJson
              .noSpaces
          )

    test(s"$name - water vapor".tag(LegacyITCTest)):
      assertAllValid(Enumerated[WaterVapor].all): wv =>
        localItc
          .calculate(
            params.copy(conditions = params.conditions.copy(wv = wv)).asJson.noSpaces
          )

    test(s"$name - sky background".tag(LegacyITCTest)):
      assertAllValid(Enumerated[SkyBackground].all): sb =>
        localItc
          .calculate(
            params.copy(conditions = params.conditions.copy(sb = sb)).asJson.noSpaces
          )

  def testSEDs(
    name:        String,
    baseParams:  ItcParameters,
    runStellar:  Boolean = true,
    runCoolStar: Boolean = true
  ): Unit =
    test(s"$name - stellar library spectrum".tag(LegacyITCTest)):
      assume(runStellar, "Skip stellar library spectrum test")
      assertAllValid(Enumerated[StellarLibrarySpectrum].all): f =>
        localItc
          .calculate(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.StellarLibrary(f)
            ).asJson.noSpaces
          )

    test(s"$name - cool star".tag(LegacyITCTest)):
      assume(runCoolStar, "Skip cool star test")
      assertAllValid(Enumerated[CoolStarTemperature].all): f =>
        localItc
          .calculate(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.CoolStarModel(f)
            ).asJson.noSpaces
          )

    test(s"$name - galaxy spectrum".tag(LegacyITCTest)):
      assertAllValid(Enumerated[GalaxySpectrum].all): f =>
        localItc
          .calculate(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.Galaxy(f)
            ).asJson.noSpaces
          )

    test(s"$name - planet spectrum".tag(LegacyITCTest)):
      assertAllValid(Enumerated[PlanetSpectrum].all): f =>
        localItc
          .calculate(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.Planet(f)
            ).asJson.noSpaces
          )

    test(s"$name - quasar spectrum".tag(LegacyITCTest)):
      assertAllValid(Enumerated[QuasarSpectrum].all): f =>
        localItc
          .calculate(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.Quasar(f)
            ).asJson.noSpaces
          )

    test(s"$name - hii region spectrum".tag(LegacyITCTest)):
      assertAllValid(Enumerated[HIIRegionSpectrum].all): f =>
        localItc
          .calculate(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.HIIRegion(f)
            ).asJson.noSpaces
          )

    test(s"$name - planetary nebula spectrum".tag(LegacyITCTest)):
      assertAllValid(Enumerated[PlanetaryNebulaSpectrum].all): f =>
        localItc
          .calculate(
            bodySED(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              UnnormalizedSED.PlanetaryNebula(f)
            ).asJson.noSpaces
          )

  def testUserDefinedSED(name: String, baseParams: ItcParameters): Unit =
    test(s"$name - user defined SED".tag(LegacyITCTest)):
      val userDefinedFluxDensities = NonEmptyMap.of(
        Wavelength.decimalNanometers.getOption(300).get -> BigDecimal(0.5),
        Wavelength.decimalNanometers.getOption(500).get -> BigDecimal(1.0),
        Wavelength.decimalNanometers.getOption(600).get -> BigDecimal(0.2),
        Wavelength.decimalNanometers.getOption(700).get -> BigDecimal(0.3)
      )

      val result = localItc
        .calculate(
          bodySED(
            baseParams.source,
            baseParams.observation,
            baseParams.conditions,
            baseParams.instrument,
            UnnormalizedSED.UserDefined(userDefinedFluxDensities)
          ).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  def testBrightnessUnits(
    name:       String,
    baseParams: ItcParameters,
    errorCheck: List[String] => Boolean = allowedErrors
  ): Unit =
    test(s"$name - brightness integrated units".tag(LegacyITCTest)):
      assertAllValid(Brightness.Integrated.all.toList, errorCheck = errorCheck): f =>
        localItc
          .calculate(
            bodyIntMagUnits(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
            ).asJson.noSpaces
          )

    test(s"$name - surface units".tag(LegacyITCTest)):
      assertAllValid(Brightness.Surface.all.toList, errorCheck = errorCheck): f =>
        localItc
          .calculate(
            bodySurfaceMagUnits(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
            ).asJson.noSpaces
          )

    test(s"$name - gaussian units".tag(LegacyITCTest)):
      assertAllValid(Brightness.Integrated.all.toList, errorCheck = errorCheck): f =>
        localItc
          .calculate(
            bodyIntGaussianMagUnits(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
            ).asJson.noSpaces
          )

  def testPowerAndBlackbody(name: String, baseParams: ItcParameters): Unit =
    test(s"$name - power law".tag(LegacyITCTest)):
      assertAllValid(List(-10, 0, 10)): f =>
        localItc
          .calculate(
            bodyPowerLaw(
              baseParams.source,
              baseParams.observation,
              baseParams.conditions,
              baseParams.instrument,
              f
            ).asJson.noSpaces
          )
