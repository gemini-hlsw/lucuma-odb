// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.tests

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.refined.auto.*
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.service.Itc
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData

object MockItc extends Itc[IO]:

  override def calculateSignalToNoise(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(exposureTime, 10.refined)),
      Band.R.asLeft,
      SignalToNoiseAt(atWavelength,
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
      ).some,
      List.empty // Empty CCD list for mock
    ).pure[IO]

  override def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(TimeSpan.fromSeconds(1).get, 10.refined)),
      Band.R.asLeft,
      SignalToNoiseAt(atWavelength,
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
      ).some,
      List.empty
    ).pure[IO]

  override def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetGraphsCalcResult] =
    TargetGraphsCalcResult(
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
        ItcGraphGroup(
          NonEmptyChain.of(
            ItcGraph(
              GraphType.S2NGraph,
              List(
                ItcSeries("title", SeriesDataType.FinalS2NData, List((1.0, 1000.0), (2.0, 1001.0)))
              )
            )
          )
        )
      ),
      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(1009.0)),
      SignalToNoise.fromInt(1001).map(TotalSN.apply(_)),
      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_)),
      Band.R.asLeft
    )
      .pure[IO]

object MockImagingItc extends Itc[IO]:

  override def calculateSignalToNoise(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(TimeSpan.fromSeconds(1).get, 10.refined)),
      Band.R.asLeft,
      SignalToNoiseAt(atWavelength,
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
      ).some,
      List(
        ItcCcd(
          SingleSN(SignalToNoise.unsafeFromBigDecimalExact(50.5)),
          Some(55.0),
          TotalSN(SignalToNoise.unsafeFromBigDecimalExact(150.5)),
          Some(160.0),
          Some(Wavelength.fromIntNanometers(800).get),
          Some(Wavelength.fromIntNanometers(750).get),
          35000.0,
          65000.0,
          2.5,
          List(
            ItcWarning(
              "Saturation. Warning: Peak pixel intensity exceeds 80% of the pixel full well depth; Peak pixel flux = 100.000%"
            )
          )
        ),
        ItcCcd(
          SingleSN(SignalToNoise.unsafeFromBigDecimalExact(45.2)),
          Some(48.0),
          TotalSN(SignalToNoise.unsafeFromBigDecimalExact(135.7)),
          Some(140.0),
          Some(Wavelength.fromIntNanometers(850).get),
          Some(Wavelength.fromIntNanometers(800).get),
          28000.0,
          65000.0,
          2.5,
          Nil
        )
      )
    ).pure[IO]

  override def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.of(
        IntegrationTime(TimeSpan.fromSeconds(1).get, 10.refined),
        IntegrationTime(TimeSpan.fromSeconds(2).get, 5.refined)
      ),
      Band.R.asLeft,
      SignalToNoiseAt(atWavelength,
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
      ).some,
      List(
        ItcCcd(
          SingleSN(SignalToNoise.unsafeFromBigDecimalExact(50.5)),
          Some(55.0),
          TotalSN(SignalToNoise.unsafeFromBigDecimalExact(150.5)),
          Some(160.0),
          Some(Wavelength.fromIntNanometers(800).get),
          Some(Wavelength.fromIntNanometers(750).get),
          35000.0,
          65000.0,
          2.5,
          List(
            ItcWarning(
              "Saturation. Warning: Peak pixel intensity exceeds 80% of the pixel full well depth; Peak pixel flux = 100.000%"
            )
          )
        ),
        ItcCcd(
          SingleSN(SignalToNoise.unsafeFromBigDecimalExact(45.2)),
          Some(48.0),
          TotalSN(SignalToNoise.unsafeFromBigDecimalExact(135.7)),
          Some(140.0),
          Some(Wavelength.fromIntNanometers(850).get),
          Some(Wavelength.fromIntNanometers(800).get),
          28000.0,
          65000.0,
          2.5,
          Nil
        )
      )
    ).pure[IO]

  override def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetGraphsCalcResult] =
    TargetGraphsCalcResult(
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
        ItcGraphGroup(
          NonEmptyChain.of(
            ItcGraph(
              GraphType.S2NGraph,
              List(
                ItcSeries("title", SeriesDataType.FinalS2NData, List((1.0, 1000.0), (2.0, 1001.0)))
              )
            )
          )
        )
      ),
      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(1009.0)),
      SignalToNoise.fromInt(1001).map(TotalSN.apply(_)),
      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_)),
      Band.R.asLeft
    )
      .pure[IO]

object EmissionLineMockItc extends Itc[IO]:

  override def calculateSignalToNoise(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(TimeSpan.fromSeconds(1).get, 10.refined)),
      Band.R.asLeft,
      SignalToNoiseAt(atWavelength,
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
      ).some,
      List.empty
    ).pure[IO]

  override def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(TimeSpan.fromSeconds(1).get, 10.refined)),
      Wavelength.unsafeFromIntPicometers(650000).asRight,
      SignalToNoiseAt(atWavelength,
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
      ).some,
      List.empty
    ).pure[IO]

  override def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetGraphsCalcResult] =
    IO.raiseError(CalculationError("Not implemented"))

object FailingMockItc extends Itc[IO]:

  override def calculateSignalToNoise(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetIntegrationTime] =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(TimeSpan.fromSeconds(1).get, 10.refined)),
      Band.R.asLeft,
      SignalToNoiseAt(atWavelength,
                      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(101.0)),
                      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(102.0))
      ).some,
      List.empty
    ).pure[IO]

  override def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): IO[TargetIntegrationTime] =
    IO.raiseError(CalculationError("A calculation error"))

  override def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): IO[TargetGraphsCalcResult] =
    TargetGraphsCalcResult(
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
        ItcGraphGroup(
          NonEmptyChain.of(
            ItcGraph(
              GraphType.S2NGraph,
              List(
                ItcSeries("title", SeriesDataType.FinalS2NData, List((1.0, 1000.0), (2.0, 1001.0)))
              )
            )
          )
        )
      ),
      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(1000.0)),
      SignalToNoise.fromInt(1001).map(TotalSN.apply(_)),
      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
      SignalToNoise.fromInt(1002).map(SingleSN.apply(_)),
      Band.R.asLeft
    )
      .pure[IO]
