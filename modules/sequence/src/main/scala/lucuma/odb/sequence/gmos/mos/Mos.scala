// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package mos

import cats.Monad
import cats.data.EitherT
import fs2.Pure
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

/**
 * GMOS MOS sequence generation.
 *
 * The science sequence is a shared GMOS spectroscopy sequence, generated exactly as it would
 * be for the long slit using the custom mask's slit width.
 */
object Mos:

  private def instantiate[F[_]: Monad, S, D](
    static:      S,
    acquisition: Either[OdbError, SequenceGenerator[D]],
    science:     F[Either[OdbError, SequenceGenerator[D]]]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, S, D]]] =
    (for
      a <- EitherT.fromEither(acquisition)
      s <- EitherT(science)
    yield StreamingExecutionConfig(static, a.generate, s.generate)).value

  def gmosNorth[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
    namespace:     UUID,
    expander:      SmartGcalExpander[F, StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
    config:        Config.GmosNorth,
    scienceItc:    Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, StaticConfig.GmosNorth, DynamicConfig.GmosNorth]]] =
    val static = InitialConfigs.GmosNorthStatic
    instantiate(
      static,
      Acquisition.gmosNorth(observationId, estimator, static, namespace, config, calRole),
      spectroscopy.Science.gmosNorth(observationId, estimator, static, namespace, expander, ObservingMode.GmosNorthMosName, config, scienceItc, calRole)
    )

  def gmosSouth[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
    namespace:     UUID,
    expander:      SmartGcalExpander[F, StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
    config:        Config.GmosSouth,
    scienceItc:    Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, StaticConfig.GmosSouth, DynamicConfig.GmosSouth]]] =
    val static = InitialConfigs.GmosSouthStatic
    instantiate(
      static,
      Acquisition.gmosSouth(observationId, estimator, static, namespace, config, calRole),
      spectroscopy.Science.gmosSouth(observationId, estimator, static, namespace, expander, ObservingMode.GmosSouthMosName, config, scienceItc, calRole)
    )

end Mos
