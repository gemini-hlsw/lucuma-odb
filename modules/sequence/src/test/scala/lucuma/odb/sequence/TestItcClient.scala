// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.*
import lucuma.core.util.TimeSpan
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcResult
import lucuma.itc.client.ItcVersions
import lucuma.itc.client.SpectroscopyModeInput
import lucuma.itc.client.SpectroscopyResult

object TestItcClient {

  def Version: ItcVersions =
    new ItcVersions("foo", "bar".some)

  def withErrorResult[F[_]: Applicative](
    msg: String
  ): ItcClient[F] =
    withResult[F](ItcResult.Error(msg))

  def withSuccessResult[F[_]: Applicative](
    exposureTime:  TimeSpan,
    exposures:     Int,
    signalToNoise: BigDecimal
  ): ItcClient[F] =
    withResult[F](
      ItcResult.Success(
        exposureTime,
        NonNegInt.unsafeFrom(exposures),
        PosBigDecimal.unsafeFrom(signalToNoise)
      )
    )

  def withResult[F[_]: Applicative](
    result: ItcResult
  ): ItcClient[F] =
    new ItcClient[F] {

      def spectroscopy(
        input:    SpectroscopyModeInput,
        useCache: Boolean
      ): F[SpectroscopyResult] =
        SpectroscopyResult(Version, result.some).pure[F]

      def versions: F[ItcVersions] =
        Version.pure[F]
    }

}
