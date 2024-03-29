// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.*
import lucuma.core.data.Zipper
import lucuma.core.math.SignalToNoise
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcCcd
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcVersions
import lucuma.itc.client.OptimizedChartResult
import lucuma.itc.client.OptimizedSpectroscopyGraphInput
import lucuma.itc.client.OptimizedSpectroscopyGraphResult
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphResult
import lucuma.itc.client.SpectroscopyIntegrationTimeInput

object TestItcClient {

  def Version: ItcVersions =
    new ItcVersions("foo", "bar".some)

  def withSuccessResult[F[_]: Applicative](
    exposureTime:  TimeSpan,
    exposures:     Int,
    signalToNoise: BigDecimal,
    graphResult: (NonEmptyList[ItcCcd], NonEmptyList[OptimizedChartResult])
  ): ItcClient[F] =
    withResult[F](
      IntegrationTime(
        exposureTime,
        PosInt.unsafeFrom(exposures),
        SignalToNoise.unsafeFromBigDecimalExact(signalToNoise)
      ),
      graphResult
    )

  def withResult[F[_]: Applicative](
    result: IntegrationTime,
    graphResult: (NonEmptyList[ItcCcd], NonEmptyList[OptimizedChartResult])
  ): ItcClient[F] =
    new ItcClient[F] {

      override def spectroscopy(
        input:    SpectroscopyIntegrationTimeInput,
        useCache: Boolean
      ): F[IntegrationTimeResult] =
        IntegrationTimeResult(Version, Zipper.fromNel(NonEmptyList.one(result))).pure[F]

      override def imaging(
        input:    ImagingIntegrationTimeInput,
        useCache: Boolean
      ): F[IntegrationTimeResult] =
        IntegrationTimeResult(
          Version,
          Zipper.of(result, result)
        ).pure[F]

      override def optimizedSpectroscopyGraph(
        input:    OptimizedSpectroscopyGraphInput,
        useCache: Boolean = true
      ): F[OptimizedSpectroscopyGraphResult] =
        ???

      override def spectroscopyIntegrationTimeAndGraph(
        input:    SpectroscopyIntegrationTimeAndGraphInput,
        useCache: Boolean = true
      ): F[SpectroscopyIntegrationTimeAndGraphResult] =
        ???

      def versions: F[ItcVersions] =
        Version.pure[F]
    }

}
