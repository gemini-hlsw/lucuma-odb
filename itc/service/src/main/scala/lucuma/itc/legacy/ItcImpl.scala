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
import lucuma.core.model.ExposureTimeMode
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
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer

import scala.concurrent.duration.*
import scala.math.*

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type ExposureCount = Int

  def build[F[_]: {MonadThrow as F, Logger as L, Tracer as T}](itcLocal: FLocalItc[F]): Itc[F] =
    new Itc[F] {

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

      def calculateGraphs(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: PosInt
      ): F[TargetGraphsCalcResult] =
        T.span("calculate graphs")
          .surround:
            observingMode match
              case s @ (SpectroscopyMode.GmosNorth(_, _, _, _, _, _, _) |
                  SpectroscopyMode.GmosSouth(_, _, _, _, _, _, _) |
                  SpectroscopyMode.Flamingos2(_, _, _, _) | SpectroscopyMode.Igrins2(_) |
                  SpectroscopyMode.Ghost(_, _, _, _)) =>
                spectroscopyGraphs(
                  target,
                  atWavelength,
                  s,
                  constraints,
                  exposureTime,
                  exposureCount
                )
              case ImagingMode.GmosNorth(_, _, _) | ImagingMode.GmosSouth(_, _, _) |
                  ImagingMode.Flamingos2(_, _) =>
                F.raiseError:
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

        T.span("spectroscopy graphs")
          .use: span =>
            for
              _      <- span.addAttributes(
                          Attribute("params.exposure_time", exposureTime.toMilliseconds.toLong),
                          Attribute("params.exposure_count", exposureCount.value.toLong),
                          Attribute("params.science_mode", "spectroscopy")
                        )
              _      <- L.info("spectroscopy graph request:")
              _      <- L.info(request.noSpaces) // Request to the legacy itc
              r      <- itcLocal.calculateGraphs(request.noSpaces, atWavelength)
              result <- T.span("convert graphs_result")
                          .surround:
                            TargetGraphsCalcResult
                              .fromLegacy(r.ccds, r.groups, atWavelength, bandOrLine)
                              .pure[F]
            yield result

      /** Calculate the Integration Time under the requested conditions. */
      def calculate(
        target:           TargetData,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureTimeMode: ExposureTimeMode
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.*

        T.span("ITC calculate")
          .use: span =>
            val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
              toItcParameters(
                target,
                observingMode,
                constraints,
                exposureTimeMode
              ).leftMap(_.asJson)

            def traceParams: F[Unit] =
              exposureTimeMode match
                case ExposureTimeMode.SignalToNoiseMode(sn, at)         =>
                  span.addAttribute(Attribute("params.signal_to_noise", sn.toBigDecimal.toDouble))
                case ExposureTimeMode.TimeAndCountMode(time, count, at) =>
                  span.addAttributes(Attribute("params.exposure_time", time.toMilliseconds.toLong),
                                     Attribute("params.exposure_count", count.value.toLong)
                  )

            for
              _      <- L.info(exposureTimeMode.desiredString)
              _      <- L.info(s"Target $target")
              _      <- traceParams
              _      <- span.addAttributes(
                          Attribute("params.at", exposureTimeMode.at.nm.value.value.toDouble),
                          Attribute("params.observing_mode", observingMode.description)
                        )
              _      <- L.info(request.noSpaces)
              a      <- itcLocal.calculate(request.noSpaces, exposureTimeMode.at)
              result <- T.span("convert integration_time_result")
                          .surround:
                            convertIntegrationTimeRemoteResult(a, bandOrLine)
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
                IntegrationTime(expTime, PosInt.unsafeFrom(r.exposureCount.value)).pure
              )
              .getOrElse:
                F.raiseError:
                  CalculationError(s"Negative exposure time ${r.exposureTime}")
          .flatMap: ccdTimes =>
            Zipper
              .of(ccdTimes.head, ccdTimes.tail.toList*)
              .focusIndex(r.exposureCalculation.selectedIndex)
              .map(_.pure)
              .getOrElse:
                F.raiseError:
                  CalculationError("Selected CCD index out of bounds")
        val convertedCcds = r.ccds.toList.flatMap(fromLegacyCcd)
        tgts.map(times =>
          TargetIntegrationTime(times, bandOrLine, r.signalToNoiseAt.map(fromLegacy), convertedCcds)
        )
    }

}
