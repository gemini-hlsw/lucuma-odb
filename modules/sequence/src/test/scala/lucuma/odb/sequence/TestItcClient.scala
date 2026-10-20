// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GnirsReadMode
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcReadMode
import lucuma.itc.ItcVersions
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult

object TestItcClient {

  def Version: ItcVersions =
    new ItcVersions("foo", "bar".some)

  // OCS echoes the requested coadds back in its result; only GNIRS requests carry any.
  private def requestedCoadds(mode: InstrumentMode): PosInt =
    mode match
      case InstrumentMode.GnirsSpectroscopy(coadds = c) => c
      case InstrumentMode.GnirsImaging(coadds = c)      => c
      case _                                             => PosInt.unsafeFrom(1)

  // OCS echoes the requested read mode back in time-and-count mode and selects its own in
  // S/N mode; the fake stands in for that selection with the exposure time rule, unless the
  // test's result already carries one.
  private def fakeReadMode(mode: InstrumentMode, exposureTime: TimeSpan): Option[ItcReadMode] =
    mode.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(_, _, _) =>
        mode match
          case InstrumentMode.GnirsSpectroscopy(readMode = r)      => ItcReadMode.Gnirs(r).some
          case InstrumentMode.GnirsImaging(readMode = r)           => ItcReadMode.Gnirs(r).some
          case InstrumentMode.Flamingos2Spectroscopy(readMode = r) => ItcReadMode.Flamingos2(r).some
          case InstrumentMode.Flamingos2Imaging(readMode = r)      => ItcReadMode.Flamingos2(r).some
          case _                                                   => None
      case ExposureTimeMode.SignalToNoiseMode(_, _)   =>
        mode match
          case InstrumentMode.GnirsSpectroscopy(readMode = _) | InstrumentMode.GnirsImaging(readMode = _)           =>
            ItcReadMode.Gnirs(GnirsReadMode.forExposureTime(exposureTime)).some
          case InstrumentMode.Flamingos2Spectroscopy(readMode = _) | InstrumentMode.Flamingos2Imaging(readMode = _) =>
            ItcReadMode.Flamingos2(Flamingos2ReadMode.forExposureTime(exposureTime)).some
          case _                                                                                                    =>
            None

  private def withFakeReadMode(mode: InstrumentMode, result: IntegrationTime): IntegrationTime =
    result.copy(readMode = result.readMode.orElse(fakeReadMode(mode, result.exposureTime)))

  def withResult[F[_]: Applicative](
    result:      IntegrationTime,
    bandOrLine:  Either[Band, Wavelength]
  ): ItcClient[F] =
    new ItcClient[F] {

      override def spectroscopy(
        input:    SpectroscopyInput,
        useCache: Boolean
      ): F[ClientCalculationResult] =
        val resultʹ =
          input.mode.exposureTimeMode match
            case ExposureTimeMode.SignalToNoiseMode(_, _)   =>
              withFakeReadMode(input.mode, result)
            case ExposureTimeMode.TimeAndCountMode(t, c, _) =>
              withFakeReadMode(input.mode, IntegrationTime(t, c, requestedCoadds(input.mode)))

        val snAt = input.mode.exposureTimeMode match
          case ExposureTimeMode.SignalToNoiseMode(sn, at) =>
            SignalToNoiseAt(
              at,
              SingleSN(sn),
              TotalSN(sn)
            ).some
          case _                                        =>
            none[SignalToNoiseAt]

        ClientCalculationResult(
          Version,
          AsterismIntegrationTimeOutcomes(
            NonEmptyChain.fromSeq(
              List.fill(input.asterism.length)(
                TargetIntegrationTimeOutcome(
                  TargetIntegrationTime(Zipper.fromNel(NonEmptyList.one(resultʹ)), bandOrLine, snAt, Nil).asRight
                )
              )
            ).get
          )
        ).pure[F]

      override def imaging(
        input:    ImagingInput,
        useCache: Boolean
      ): F[ClientCalculationResult] =
        ClientCalculationResult(
          Version,
          AsterismIntegrationTimeOutcomes(
            NonEmptyChain.fromSeq(
              List.fill(input.asterism.length)(
                TargetIntegrationTimeOutcome(
                  TargetIntegrationTime(Zipper.of(withFakeReadMode(input.mode, result), withFakeReadMode(input.mode, result)), bandOrLine, None, Nil).asRight
                )
              )
            ).get
          )
        ).pure[F]

      override def spectroscopyIntegrationTimeAndGraphs(
        input:    SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean = true
      ): F[SpectroscopyIntegrationTimeAndGraphsResult] =
        ???

      def versions: F[ItcVersions] =
        Version.pure[F]
    }

}
