// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package imaging

import cats.data.EitherT
import cats.effect.Sync
import fs2.Pure
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.data.ItcScience.GnirsImaging
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object Imaging:

  private def instantiate[F[_]: Sync](
    observationId:  Observation.Id,
    estimator:      StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:         GnirsStaticConfig,
    namespace:      UUID,
    config:         Config,
    acquisitionItc: Either[OdbError, IntegrationTime],
    pinnedAcqType:  Option[GnirsAcquisitionType],
    science:        F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, GnirsStaticConfig, GnirsDynamicConfig]]] =
    (for
      a <- EitherT(Acquisition.instantiate(observationId, estimator, static, namespace, config, acquisitionItc, pinnedAcqType))
      s <- EitherT(science)
    yield StreamingExecutionConfig(static, a.generate, s.generate)).value

  def gnirs[F[_]: Sync](
    observationId:  Observation.Id,
    estimator:      StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    namespace:      UUID,
    config:         Config,
    acquisitionItc: Either[OdbError, IntegrationTime],
    pinnedAcqType:  Option[GnirsAcquisitionType],
    scienceItc:     Either[OdbError, GnirsImaging]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, GnirsStaticConfig, GnirsDynamicConfig]]] =
    val static = config.staticConfig
    instantiate(
      observationId,
      estimator,
      static,
      namespace,
      config,
      acquisitionItc,
      pinnedAcqType,
      Science.gnirs(estimator, static, namespace, config, scienceItc)
    )
