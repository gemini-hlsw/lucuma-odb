// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import buildinfo.BuildInfo
import cats.Order.given
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GalaxySpectrum.Spiral
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.BrightnessUnits.Brightness
import lucuma.core.math.BrightnessUnits.FluxDensityContinuum
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessUnits.LineFlux
import lucuma.core.math.BrightnessValue
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.TaggedUnit
import lucuma.core.math.units.KilometersPerSecond
import lucuma.core.math.units.VegaMagnitude
import lucuma.core.math.units.WattsPerMeter2
import lucuma.core.math.units.WattsPerMeter2Micrometer
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ElevationRange
import lucuma.core.model.EmissionLine
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.core.model.UnnormalizedSED.Galaxy
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.refined.auto.*
import lucuma.core.util.*
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.CloudExtinctionInput
import lucuma.itc.GraphType
import lucuma.itc.ImageQualityInput
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcAxis
import lucuma.itc.ItcCcd
import lucuma.itc.ItcVersions
import lucuma.itc.SeriesDataType
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.service.ItcMapping.versionDateTimeFormatter

import java.time.Instant
import scala.collection.immutable.SortedMap

val atWavelength = Wavelength.fromIntNanometers(600).get

class WiringSuite extends ClientSuite:
  val selected = IntegrationTime(
    TimeSpan.FromString.getOption("PT1S").get,
    PosInt.unsafeFrom(10)
  )

  test("ItcClient spectroscopy basic wiring and sanity check"):
    spectroscopy(
      WiringSuite.GmosSpectroscopyInputData,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient flamingos2 spectroscopy basic wiring and sanity check"):
    spectroscopy(
      WiringSuite.Flamingos2SpectroscopyInputData,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient imaging basic wiring and sanity check"):
    imaging(
      WiringSuite.GmosImagingInputData,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient imaging f2 basic wiring and sanity check for s/n"):
    imaging(
      WiringSuite.Flamingos2ImagingInputData,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient imaging f2 basic wiring and sanity check for txc"):
    val toITC = ImagingInput.parameters
      .andThen(ImagingParameters.exposureTimeMode)
      .replace(
        ExposureTimeMode
          .TimeAndCountMode(TimeSpan.fromSeconds(1).get, 10.refined, atWavelength)
      )(WiringSuite.Flamingos2ImagingInputData)
    imaging(
      toITC,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient spectroscopy graph wiring and sanity check"):
    spectroscopyGraphs(
      WiringSuite.GraphInput,
      SpectroscopyGraphsResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismTargetGraphsResultOutcomes:
          NonEmptyChain.of(
            TargetGraphsResultOutcome:
              TargetGraphsResult(
                TargetGraphs(
                  NonEmptyChain.of(
                    ItcCcd(
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1)),
                      Some(1.0),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(2)),
                      Some(2.0),
                      Some(Wavelength.fromIntNanometers(1001).get),
                      Some(Wavelength.fromIntNanometers(1001).get),
                      3,
                      4,
                      5,
                      Nil
                    )
                  ),
                  NonEmptyChain.of(
                    GraphResult(
                      GraphType.S2NGraph,
                      List(
                        SeriesResult(
                          "title",
                          SeriesDataType.FinalS2NData,
                          List(1000.0, 1001.0),
                          ItcAxis(1, 2, 1, 2, 2).some,
                          ItcAxis(1000.0, 1001.0, 1000, 1001, 2).some
                        )
                      )
                    )
                  ),
                  TotalSN(SignalToNoise.unsafeFromBigDecimalExact(1009.0)),
                  SignalToNoise.fromInt(1001).map(TotalSN(_)),
                  SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
                  SignalToNoise.fromInt(1002).map(SingleSN(_))
                ),
                Band.R
              ).asRight
          )
      ).asRight
    ).asRight

  test("ItcClient spectroscopy with exposure time mode"):
    val toTC = SpectroscopyInput.parameters
      .andThen(SpectroscopyParameters.exposureTimeMode)
      .replace(
        ExposureTimeMode
          .TimeAndCountMode(TimeSpan.fromSeconds(1).get, 10.refined, atWavelength)
      )(WiringSuite.GmosSpectroscopyInputData)
    spectroscopy(
      toTC,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient spectroscopy with emission lines basic wiring and sanity check"):
    spectroscopyEmissionLines(
      WiringSuite.SpectroscopyEmissionLinesInput,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Wavelength.unsafeFromIntPicometers(650000).asRight,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  val selectedLarge = IntegrationTime(
    TimeSpan.FromString.getOption("PT1000000S").get,
    PosInt.unsafeFrom(10)
  )

  test("ItcClient spectroscopy with large exposure time, shortcut 5331"):
    val toTC = SpectroscopyInput.parameters
      .andThen(SpectroscopyParameters.exposureTimeMode)
      .replace(
        ExposureTimeMode
          .TimeAndCountMode(TimeSpan.fromSeconds(1000000).get, 10.refined, atWavelength)
      )(WiringSuite.GmosSpectroscopyInputData)
    spectroscopy(
      toTC,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selectedLarge)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient spectroscopy with exact imageQuality and cloudExtinction values"):
    spectroscopy(
      WiringSuite.GmosSpectroscopyExactValuesInputData,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient imaging with exact constraint values"):
    imaging(
      WiringSuite.GmosImagingExactValuesInputData,
      ClientCalculationResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismIntegrationTimeOutcomes:
          NonEmptyChain:
            TargetIntegrationTimeOutcome:
              TargetIntegrationTime(
                Zipper.fromNel(NonEmptyList.one(selected)),
                Band.R.asLeft,
                SignalToNoiseAt(atWavelength,
                                SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                                TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
                ).some,
                List.empty
              ).asRight
      ).asRight
    )

  test("ItcClient spectroscopy graphs with mixed preset and exact values"):
    spectroscopyGraphs(
      WiringSuite.GraphExactValuesInput,
      SpectroscopyGraphsResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        AsterismTargetGraphsResultOutcomes:
          NonEmptyChain.of(
            TargetGraphsResultOutcome:
              TargetGraphsResult(
                TargetGraphs(
                  NonEmptyChain.of(
                    ItcCcd(
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1)),
                      Some(1.0),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(2)),
                      Some(2.0),
                      Some(Wavelength.fromIntNanometers(1001).get),
                      Some(Wavelength.fromIntNanometers(1001).get),
                      3,
                      4,
                      5,
                      Nil
                    )
                  ),
                  NonEmptyChain.of(
                    GraphResult(
                      GraphType.S2NGraph,
                      List(
                        SeriesResult(
                          "title",
                          SeriesDataType.FinalS2NData,
                          List(1000.0, 1001.0),
                          ItcAxis(1, 2, 1, 2, 2).some,
                          ItcAxis(1000.0, 1001.0, 1000, 1001, 2).some
                        )
                      )
                    )
                  ),
                  TotalSN(SignalToNoise.unsafeFromBigDecimalExact(1009.0)),
                  SignalToNoise.fromInt(1001).map(TotalSN(_)),
                  SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
                  SignalToNoise.fromInt(1002).map(SingleSN(_))
                ),
                Band.R
              ).asRight
          )
      ).asRight
    ).asRight

object WiringSuite:

  val GmosSpectroscopyInputData: SpectroscopyInput =
    SpectroscopyInput(
      SpectroscopyParameters(
        ExposureTimeMode.SignalToNoiseMode(
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
          atWavelength
        ),
        ItcConstraintsInput(
          ImageQualityInput.preset(ImageQuality.Preset.PointOne),
          CloudExtinctionInput.preset(CloudExtinction.Preset.PointOne),
          skyBackground = SkyBackground.Darkest,
          waterVapor = WaterVapor.VeryDry,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.GmosNorthSpectroscopy(
          Wavelength.Min,
          GmosNorthGrating.B1200_G5301,
          GmosNorthFilter.GPrime.some,
          GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
          GmosCcdMode(
            GmosXBinning.Two,
            GmosYBinning.Two,
            GmosAmpCount.Twelve,
            GmosAmpGain.High,
            GmosAmpReadMode.Fast
          ).some,
          GmosRoi.FullFrame.some
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val Flamingos2SpectroscopyInputData: SpectroscopyInput =
    SpectroscopyInput(
      SpectroscopyParameters(
        ExposureTimeMode.SignalToNoiseMode(
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
          atWavelength
        ),
        ItcConstraintsInput(
          ImageQualityInput.preset(ImageQuality.Preset.PointOne),
          CloudExtinctionInput.preset(CloudExtinction.Preset.PointOne),
          skyBackground = SkyBackground.Darkest,
          waterVapor = WaterVapor.VeryDry,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.Flamingos2Spectroscopy(
          Flamingos2Disperser.R3000,
          Flamingos2Filter.J,
          Flamingos2Fpu.LongSlit2
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val Flamingos2ImagingInputData: ImagingInput =
    ImagingInput(
      ImagingParameters(
        ExposureTimeMode.SignalToNoiseMode(
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
          atWavelength
        ),
        ItcConstraintsInput(
          ImageQualityInput.preset(ImageQuality.Preset.PointOne),
          CloudExtinctionInput.preset(CloudExtinction.Preset.PointOne),
          skyBackground = SkyBackground.Darkest,
          waterVapor = WaterVapor.VeryDry,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.Flamingos2Imaging(Flamingos2Filter.J)
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val GmosImagingInputData: ImagingInput =
    ImagingInput(
      ImagingParameters(
        ExposureTimeMode.SignalToNoiseMode(
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
          atWavelength
        ),
        ItcConstraintsInput(
          ImageQualityInput.preset(ImageQuality.Preset.PointOne),
          CloudExtinctionInput.preset(CloudExtinction.Preset.PointOne),
          skyBackground = SkyBackground.Darkest,
          waterVapor = WaterVapor.VeryDry,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.GmosNorthImaging(
          GmosNorthFilter.GPrime,
          none
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val GraphInput: SpectroscopyGraphsInput =
    SpectroscopyGraphsInput(
      SpectroscopyGraphParameters(
        Wavelength.Min,
        TimeSpan.fromSeconds(1).get,
        PosInt.unsafeFrom(5),
        ItcConstraintsInput(
          ImageQualityInput.preset(ImageQuality.Preset.PointOne),
          CloudExtinctionInput.preset(CloudExtinction.Preset.PointOne),
          skyBackground = SkyBackground.Darkest,
          waterVapor = WaterVapor.VeryDry,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.GmosNorthSpectroscopy(
          Wavelength.Min,
          GmosNorthGrating.B1200_G5301,
          GmosNorthFilter.GPrime.some,
          GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
          GmosCcdMode(
            GmosXBinning.Two,
            GmosYBinning.Two,
            GmosAmpCount.Twelve,
            GmosAmpGain.High,
            GmosAmpReadMode.Fast
          ).some,
          GmosRoi.FullFrame.some
        ),
        Some(SignificantFiguresInput(2.refined, 2.refined, 2.refined))
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val SpectroscopyEmissionLinesInput: SpectroscopyInput =
    SpectroscopyInput(
      SpectroscopyParameters(
        ExposureTimeMode.SignalToNoiseMode(
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
          atWavelength
        ),
        ItcConstraintsInput(
          ImageQualityInput.preset(ImageQuality.Preset.PointOne),
          CloudExtinctionInput.preset(CloudExtinction.Preset.PointOne),
          skyBackground = SkyBackground.Darkest,
          waterVapor = WaterVapor.VeryDry,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.GmosNorthSpectroscopy(
          Wavelength.Min,
          GmosNorthGrating.B1200_G5301,
          GmosNorthFilter.GPrime.some,
          GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
          GmosCcdMode(
            GmosXBinning.Two,
            GmosYBinning.Two,
            GmosAmpCount.Twelve,
            GmosAmpGain.High,
            GmosAmpReadMode.Fast
          ).some,
          GmosRoi.FullFrame.some
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            EmissionLines[Integrated](
              SortedMap(
                Wavelength.unsafeFromIntPicometers(650000) ->
                  EmissionLine(
                    LineWidthValue.unsafeFrom(BigDecimal(1.0)).withUnit[KilometersPerSecond],
                    Measure(
                      LineFluxValue.unsafeFrom(BigDecimal(0.5)),
                      TaggedUnit[WattsPerMeter2, LineFlux[Integrated]].unit
                    ).tag
                  )
              ),
              Measure(
                FluxDensityContinuumValue.unsafeFrom(BigDecimal(0.5)),
                TaggedUnit[WattsPerMeter2Micrometer, FluxDensityContinuum[Integrated]].unit
              ).tag
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  // Test data using exact values for imageQuality and cloudExtinction
  val GmosSpectroscopyExactValuesInputData: SpectroscopyInput =
    SpectroscopyInput(
      SpectroscopyParameters(
        ExposureTimeMode.SignalToNoiseMode(
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
          atWavelength
        ),
        ItcConstraintsInput(
          ImageQualityInput.arcsec(BigDecimal("0.85")),
          CloudExtinctionInput.extinction(BigDecimal("0.3")),
          skyBackground = SkyBackground.Darkest,
          waterVapor = WaterVapor.VeryDry,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.GmosNorthSpectroscopy(
          Wavelength.Min,
          GmosNorthGrating.B1200_G5301,
          GmosNorthFilter.GPrime.some,
          GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
          GmosCcdMode(
            GmosXBinning.Two,
            GmosYBinning.Two,
            GmosAmpCount.Twelve,
            GmosAmpGain.High,
            GmosAmpReadMode.Fast
          ).some,
          GmosRoi.FullFrame.some
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val GmosImagingExactValuesInputData: ImagingInput =
    ImagingInput(
      ImagingParameters(
        ExposureTimeMode.SignalToNoiseMode(
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
          atWavelength
        ),
        ItcConstraintsInput(
          ImageQualityInput.arcsec(BigDecimal("1.2")),
          CloudExtinctionInput.extinction(BigDecimal("0.1")),
          skyBackground = SkyBackground.Bright,
          waterVapor = WaterVapor.Wet,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.GmosNorthImaging(
          GmosNorthFilter.GPrime,
          none
        )
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )

  val GraphExactValuesInput: SpectroscopyGraphsInput =
    SpectroscopyGraphsInput(
      SpectroscopyGraphParameters(
        Wavelength.Min,
        TimeSpan.fromSeconds(1).get,
        PosInt.unsafeFrom(5),
        ItcConstraintsInput(
          ImageQualityInput.arcsec(BigDecimal("0.7")),
          CloudExtinctionInput.preset(CloudExtinction.Preset.PointFive),
          skyBackground = SkyBackground.Dark,
          waterVapor = WaterVapor.Median,
          elevationRange = ElevationRange.ByAirMass.Default
        ),
        InstrumentMode.GmosNorthSpectroscopy(
          Wavelength.Min,
          GmosNorthGrating.B1200_G5301,
          GmosNorthFilter.GPrime.some,
          GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
          GmosCcdMode(
            GmosXBinning.Two,
            GmosYBinning.Two,
            GmosAmpCount.Twelve,
            GmosAmpGain.High,
            GmosAmpReadMode.Fast
          ).some,
          GmosRoi.FullFrame.some
        ),
        Some(SignificantFiguresInput(2.refined, 2.refined, 2.refined))
      ),
      NonEmptyList.of(
        TargetInput(
          SourceProfile.Point(
            BandNormalized[Integrated](
              Galaxy(Spiral).some,
              SortedMap(
                Band.R ->
                  Measure(
                    BrightnessValue.unsafeFrom(BigDecimal(10.0)),
                    TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
                  ).tag
              )
            )
          ),
          RadialVelocity.fromMetersPerSecond.getOption(1.0).get
        )
      )
    )
