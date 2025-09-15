// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcVersions
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.client.ClientCalculationResult
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
          input.exposureTimeMode match
            case ExposureTimeMode.SignalToNoiseMode(_, _)   =>
              result
            case ExposureTimeMode.TimeAndCountMode(t, c, _) =>
              IntegrationTime(t, c)

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
                  TargetIntegrationTime(Zipper.of(result, result), bandOrLine, None, Nil).asRight
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
