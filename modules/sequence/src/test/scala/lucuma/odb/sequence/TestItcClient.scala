// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.*
import lucuma.core.math.SignalToNoise
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcVersions
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.SpectroscopyResult

object TestItcClient {

  def Version: ItcVersions =
    new ItcVersions("foo", "bar".some)

  def withSuccessResult[F[_]: Applicative](
    exposureTime:  TimeSpan,
    exposures:     Int,
    signalToNoise: BigDecimal
  ): ItcClient[F] =
    withResult[F](
      IntegrationTime(
        exposureTime,
        PosInt.unsafeFrom(exposures),
        SignalToNoise.unsafeFromBigDecimalExact(signalToNoise)
      )
    )

  def withResult[F[_]: Applicative](
    result: IntegrationTime
  ): ItcClient[F] =
    new ItcClient[F] {

      def spectroscopy(
        input:    SpectroscopyIntegrationTimeInput,
        useCache: Boolean
      ): F[SpectroscopyResult] =
        SpectroscopyResult(Version, result.some).pure[F]

      def optimizedSpectroscopyGraph(input: lucuma.itc.client.OptimizedSpectroscopyGraphInput, useCache: Boolean): F[lucuma.itc.client.OptimizedSpectroscopyGraphResult] =
        ???

      def versions: F[ItcVersions] =
        Version.pure[F]
    }

}
