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
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcCcd
import lucuma.itc.ItcVersions
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.client.GraphResult
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyGraphsInput
import lucuma.itc.client.SpectroscopyGraphsResult
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult
import lucuma.itc.client.SpectroscopyIntegrationTimeInput

object TestItcClient {

  def Version: ItcVersions =
    new ItcVersions("foo", "bar".some)

  def withSuccessResult[F[_]: Applicative](
    exposureTime:  TimeSpan,
    exposureCount: Int,
    signalToNoise: BigDecimal,
    band:          Band,
    graphResult:   (NonEmptyList[ItcCcd], NonEmptyList[GraphResult])
  ): ItcClient[F] =
    withResult[F](
      IntegrationTime(
        exposureTime,
        PosInt.unsafeFrom(exposureCount),
        SignalToNoise.unsafeFromBigDecimalExact(signalToNoise)
      ),
      band,
      graphResult
    )

  def withResult[F[_]: Applicative](
    result:      IntegrationTime,
    band:        Band,
    graphResult: (NonEmptyList[ItcCcd], NonEmptyList[GraphResult])
  ): ItcClient[F] =
    new ItcClient[F] {

      override def spectroscopy(
        input:    SpectroscopyIntegrationTimeInput,
        useCache: Boolean
      ): F[IntegrationTimeResult] =
        IntegrationTimeResult(
          Version,
          AsterismIntegrationTimeOutcomes(
            NonEmptyChain.fromSeq(
              List.fill(input.asterism.length)(
                TargetIntegrationTimeOutcome(
                  TargetIntegrationTime(Zipper.fromNel(NonEmptyList.one(result)), band).asRight
                )
              )
            ).get
          )
        ).pure[F]

      override def imaging(
        input:    ImagingIntegrationTimeInput,
        useCache: Boolean
      ): F[IntegrationTimeResult] =
        IntegrationTimeResult(
          Version,
          AsterismIntegrationTimeOutcomes(
            NonEmptyChain.fromSeq(
              List.fill(input.asterism.length)(
                TargetIntegrationTimeOutcome(
                  TargetIntegrationTime(Zipper.of(result, result), band).asRight
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
