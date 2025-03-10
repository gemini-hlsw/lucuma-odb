// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcCcd
import lucuma.itc.ItcVersions
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.SingleSN
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.TotalSN
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.GraphResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyGraphsInput
import lucuma.itc.client.SpectroscopyGraphsResult
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult

object TestItcClient {

  def Version: ItcVersions =
    new ItcVersions("foo", "bar".some)

  def withSuccessResult[F[_]: Applicative](
    exposureTime:  TimeSpan,
    exposureCount: Int,
    bandOrLine:    Either[Band, Wavelength],
    graphResult:   (NonEmptyList[ItcCcd], NonEmptyList[GraphResult])
  ): ItcClient[F] =
    withResult[F](
      IntegrationTime(
        exposureTime,
        NonNegInt.unsafeFrom(exposureCount),
      ),
      bandOrLine,
      graphResult
    )

  def withResult[F[_]: Applicative](
    result:      IntegrationTime,
    bandOrLine:  Either[Band, Wavelength],
    graphResult: (NonEmptyList[ItcCcd], NonEmptyList[GraphResult])
  ): ItcClient[F] =
    new ItcClient[F] {

      override def spectroscopy(
        input:    SpectroscopyInput,
        useCache: Boolean
      ): F[ClientCalculationResult] =
        val resultʹ =
          input.exposureTimeMode match
            case ExposureTimeMode.SignalToNoiseMode(_, _)   =>
              result
            case ExposureTimeMode.TimeAndCountMode(t, c, _) =>
              IntegrationTime(t, NonNegInt.unsafeFrom(c.value))

        val snAt = input.exposureTimeMode match
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
                  TargetIntegrationTime(Zipper.fromNel(NonEmptyList.one(resultʹ)), bandOrLine, snAt).asRight
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
                  TargetIntegrationTime(Zipper.of(result, result), bandOrLine, None).asRight
                )
              )
            ).get
          )
        ).pure[F]

      override def spectroscopyGraphs(
        input:    SpectroscopyGraphsInput,
        useCache: Boolean = true
      ): F[SpectroscopyGraphsResult] =
        ???

      override def spectroscopyIntegrationTimeAndGraphs(
        input:    SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean = true
      ): F[SpectroscopyIntegrationTimeAndGraphsResult] =
        ???

      def versions: F[ItcVersions] =
        Version.pure[F]
    }

}
