// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package imaging

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all.*
import fs2.Pure
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsReadMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.ItcScience.GnirsImaging
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

/**
 * The time-and-count read mode when none is explicit: Bright, or Very Bright below its
 * minimum exposure (https://app.shortcut.com/lucuma/story/10909). Faint is only ever an
 * explicit choice. In signal-to-noise mode the ITC selects the read mode instead.
 */
def defaultReadMode(exposureTime: TimeSpan): GnirsReadMode =
  if exposureTime < GnirsReadMode.Bright.minimumExposureTime then GnirsReadMode.VeryBright
  else GnirsReadMode.Bright

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
