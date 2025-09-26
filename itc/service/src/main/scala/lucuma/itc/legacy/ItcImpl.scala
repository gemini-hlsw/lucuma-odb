// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.*
import cats.syntax.all.*
import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.CalculationError
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcCcd
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.TargetGraphsCalcResult
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.Itc
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.ObservingMode.ImagingMode
import lucuma.itc.service.ObservingMode.SpectroscopyMode
import lucuma.itc.service.TargetData
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.math.*

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type ExposureCount = Int

  def build[F[_]: MonadThrow: Logger: Trace](itcLocal: FLocalItc[F]): Itc[F] =
    new Itc[F] {
      val T = Trace[F]
      val L = Logger[F]

      private def fromLegacy(sn: lucuma.itc.legacy.SignalToNoiseAt): SignalToNoiseAt =
        SignalToNoiseAt(sn.wavelength, sn.single, sn.total)

      private def fromLegacyCcd(remoteCcd: ItcRemoteCcd): Option[ItcCcd] =
        for
          single <- SignalToNoise.FromBigDecimalRounding.getOption(remoteCcd.singleSNRatio)
          total  <- SignalToNoise.FromBigDecimalRounding.getOption(remoteCcd.totalSNRatio)
        yield ItcCcd(
          singleSNRatio = SingleSN(single),
          maxSingleSNRatio = None,
          totalSNRatio = TotalSN(total),
          maxTotalSNRatio = None,
          wavelengthForMaxTotalSNRatio = None,
          wavelengthForMaxSingleSNRatio = None,
          peakPixelFlux = remoteCcd.peakPixelFlux,
          wellDepth = remoteCcd.wellDepth,
          ampGain = remoteCcd.ampGain,
          warnings = remoteCcd.warnings
        )

      def calculateIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        T.span("calculate integration_time"):
          observingMode match
            case s @ (SpectroscopyMode.GmosNorth(_, _, _, _, _, _) |
                SpectroscopyMode.GmosSouth(_, _, _, _, _, _) |
                SpectroscopyMode.Flamingos2(_, _, _)) =>
              spectroscopyIntegrationTime(target, atWavelength, s, constraints, signalToNoise)
            case i @ (
                  ObservingMode.ImagingMode.GmosNorth(_, _) |
                  ObservingMode.ImagingMode.GmosSouth(_, _) |
                  ObservingMode.ImagingMode.Flamingos2(_)
                ) =>
              imagingIntegrationTime(target, atWavelength, i, constraints, signalToNoise)

      def calculateGraphs(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: PosInt
      ): F[TargetGraphsCalcResult] =
        T.span("calculate graphs"):
          observingMode match
            case s @ (SpectroscopyMode.GmosNorth(_, _, _, _, _, _) |
                SpectroscopyMode.GmosSouth(_, _, _, _, _, _) |
                SpectroscopyMode.Flamingos2(_, _, _)) =>
              spectroscopyGraphs(
                target,
                atWavelength,
                s,
                constraints,
                exposureTime,
                exposureCount
              )
            case ImagingMode.GmosNorth(_, _) | ImagingMode.GmosSouth(_, _) |
                ImagingMode.Flamingos2(_) =>
              MonadThrow[F].raiseError:
                new IllegalArgumentException("Imaging mode not supported for graph calculation")

      private def spectroscopyGraphs(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: PosInt
      ): F[TargetGraphsCalcResult] =
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          spectroscopyGraphParams(
            target,
            atWavelength,
            observingMode,
            exposureTime.toMilliseconds.toDouble.milliseconds,
            constraints,
            exposureCount.value
          ).leftMap(_.asJson)

        for
          _ <- T.put("params.exposure_time" -> exposureTime.toMilliseconds.toInt)
          _ <- T.put("params.exposure_count" -> exposureCount.value)
          _ <- T.put("params.science_mode" -> "spectroscopy")
          _ <- L.info("spectroscopy graph request:")
          _ <- L.info(request.noSpaces) // Request to the legacy itc
          r <- itcLocal.calculateGraphs(request.noSpaces)
        yield TargetGraphsCalcResult.fromLegacy(r.ccds, r.groups, atWavelength, bandOrLine)

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes.
       */
      private def spectroscopyIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.SpectroscopyMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          spectroscopyIntegrationTimeParams(
            target,
            atWavelength,
            observingMode,
            constraints,
            signalToNoise
          ).leftMap(_.asJson)

        for
          _      <- L.info(s"Desired S/N $signalToNoise")
          _      <- L.info(s"Target $target at wavelength $atWavelength")
          _      <- T.put("params.signal_to_noise" -> signalToNoise.toBigDecimal.toDouble)
          _      <- T.put("params.at" -> atWavelength.nm.value.value.toDouble)
          _      <- T.put("params.science_mode" -> "spectroscopy")
          _      <- L.info(request.noSpaces) // Request to the legacy itc
          _      <- L.info("spectroscopy time request:")
          a      <- itcLocal.calculateIntegrationTime(request.noSpaces)
          result <- convertIntegrationTimeRemoteResult(a, bandOrLine)
        yield result

      private def convertIntegrationTimeRemoteResult(
        r:          IntegrationTimeRemoteResult,
        bandOrLine: Either[Band, Wavelength]
      ): F[TargetIntegrationTime] =
        val tgts          = r.exposureCalculation.exposures
          .traverse: r =>
            TimeSpan
              .fromSeconds(r.exposureTime)
              .map(expTime =>
                IntegrationTime(expTime, PosInt.unsafeFrom(r.exposureCount.value))
                  .pure[F]
              )
              .getOrElse:
                MonadThrow[F].raiseError:
                  CalculationError(s"Negative exposure time ${r.exposureTime}")
          .flatMap: ccdTimes =>
            Zipper
              .of(ccdTimes.head, ccdTimes.tail.toList*)
              .focusIndex(r.exposureCalculation.selectedIndex)
              .map(_.pure[F])
              .getOrElse:
                MonadThrow[F].raiseError:
                  CalculationError("Selected CCD index out of bounds")
        val convertedCcds = r.ccds.toList.flatMap(fromLegacyCcd)
        tgts.map(times =>
          TargetIntegrationTime(times, bandOrLine, r.signalToNoiseAt.map(fromLegacy), convertedCcds)
        )

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes.
       */
      private def spectroscopySignalToNoise(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.SpectroscopyMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: PosInt
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          spectroscopySNParams(
            target,
            atWavelength,
            observingMode,
            constraints,
            exposureTime.toMilliseconds.toDouble.milliseconds,
            exposureCount.value
          ).leftMap(_.asJson)

        for
          _ <- L.info(s"Calculate S/N for exp time $exposureTime and count $exposureCount")
          _ <- L.info(s"Target $target at wavelength $atWavelength")
          _ <- T.put("params.science_mode" -> "spectroscopy")
          _ <- T.put("params.exposure_time" -> exposureTime.toMilliseconds.toInt)
          _ <- T.put("params.exposure_count" -> exposureCount.value)
          _ <- L.info(
                 s"Spectroscopy: Signal to noise mode ${request.noSpaces}"
               ) // Request to the legacy itc
          a <- itcLocal.calculateSignalToNoise(request.noSpaces)
        yield TargetIntegrationTime(
          Zipper.one(IntegrationTime(exposureTime, exposureCount)),
          bandOrLine,
          a.signalToNoiseAt.map(fromLegacy),
          a.ccds.toList.flatMap(fromLegacyCcd)
        )

      def calculateSignalToNoise(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: PosInt
      ): F[TargetIntegrationTime] =
        T.span("calculate signal_to_noise"):
          observingMode match
            case s @ (ObservingMode.SpectroscopyMode.GmosNorth(_, _, _, _, _, _) |
                ObservingMode.SpectroscopyMode.GmosSouth(_, _, _, _, _, _) |
                ObservingMode.SpectroscopyMode.Flamingos2(_, _, _)) =>
              spectroscopySignalToNoise(target,
                                        atWavelength,
                                        s,
                                        constraints,
                                        exposureTime,
                                        exposureCount
              )
            case s @ (ObservingMode.ImagingMode.Flamingos2(_) |
                ObservingMode.ImagingMode.GmosSouth(_, _) |
                ObservingMode.ImagingMode.GmosNorth(_, _)) =>
              imagingSignalToNoise(target,
                                   atWavelength,
                                   s,
                                   constraints,
                                   exposureTime,
                                   exposureCount
              )

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      private def imagingIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.ImagingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          imagingIntegrationTimeParams(target,
                                       atWavelength,
                                       observingMode,
                                       constraints,
                                       signalToNoise
          ).leftMap(
            _.asJson
          )

        for
          _            <- L.info(s"Desired S/N $signalToNoise")
          _            <- L.info(s"Target $target  at wavelength $atWavelength")
          _            <- T.put("params.signal_to_noise" -> signalToNoise.toBigDecimal.toDouble)
          _            <- T.put("params.at" -> atWavelength.nm.value.value.toDouble)
          _            <- T.put("params.science_mode" -> "imaging")
          // Request to the legacy itc
          _            <- L.info(s"Imaging: Signal to noise mode ${request.noSpaces}")
          remoteResult <- itcLocal.calculateIntegrationTime(request.noSpaces)
          result       <- convertIntegrationTimeRemoteResult(remoteResult, bandOrLine)
        yield result

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      private def imagingSignalToNoise(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.ImagingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: PosInt
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          imagingS2NParams(
            target,
            atWavelength,
            observingMode,
            constraints,
            exposureTime.toMilliseconds.toDouble.milliseconds,
            exposureCount.value
          ).leftMap(_.asJson)

        for {
          _            <- L.info(s"Calculate S/N for exp time $exposureTime and count $exposureCount")
          _            <- L.info(s"Target $target  at wavelength $atWavelength")
          _            <- T.put("params.science_mode" -> "imaging")
          _            <- T.put("params.exposure_time" -> exposureTime.toMilliseconds.toInt)
          _            <- T.put("params.exposure_count" -> exposureCount.value)
          // Request to the legacy itc
          _            <- L.info(s"Imaging: time and count mode ${request.noSpaces}")
          remoteResult <- itcLocal.calculateIntegrationTime(request.noSpaces)
          result       <- convertIntegrationTimeRemoteResult(remoteResult, bandOrLine)
        } yield result
    }

}
