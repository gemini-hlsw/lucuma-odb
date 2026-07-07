// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package imaging

import cats.data.EitherT
import cats.effect.Sync
import fs2.Pure
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.odb.data.Itc.GnirsImaging
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object Imaging:

  private def instantiate[F[_]: Sync](
    static:  GnirsStaticConfig,
    science: F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, GnirsStaticConfig, GnirsDynamicConfig]]] =
    EitherT(science)
      // Note acquisition is empty for imaging
      .map(s => StreamingExecutionConfig(static, SequenceGenerator.empty.generate, s.generate))
      .value

  def gnirs[F[_]: Sync](
    estimator:  StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    namespace:  UUID,
    config:     Config,
    scienceItc: Either[OdbError, GnirsImaging]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, GnirsStaticConfig, GnirsDynamicConfig]]] =
    val static = config.staticConfig
    instantiate(
      static,
      Science.gnirs(estimator, static, namespace, config, scienceItc)
    )
