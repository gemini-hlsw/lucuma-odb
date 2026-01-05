// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Monad
import cats.data.EitherT
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.Timestamp
import lucuma.odb.data.Itc.Spectroscopy
import lucuma.odb.data.OdbError

import java.util.UUID

object LongSlit:

  private def instantiate[F[_]: Monad, S, D](
    static:      S,
    acquisition: Either[OdbError, SequenceGenerator[D]],
    science:     F[Either[OdbError, SequenceGenerator[D]]]
  ): F[Either[OdbError, ExecutionConfigGenerator[S, D]]] =
    (for
      a <- EitherT.fromEither(acquisition)
      s <- EitherT(science)
    yield ExecutionConfigGenerator(static, a, s)).value

  def gmosNorth[F[_]: Monad](
    observationId:  Observation.Id,
    estimator:      TimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
    namespace:      UUID,
    expander:       SmartGcalExpander[F, DynamicConfig.GmosNorth],
    config:         Config.GmosNorth,
    itc:            Either[OdbError, Spectroscopy],
    calRole:        Option[CalibrationRole],
    lastAcqReset:   Option[Timestamp]
  ): F[Either[OdbError, ExecutionConfigGenerator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]]] =
    val static = InitialConfigs.GmosNorthStatic
    instantiate(
      static,
      Acquisition.gmosNorth(observationId, estimator, static, namespace, config, itc.map(_.acquisition.focus.value), calRole, lastAcqReset),
      Science.gmosNorth(observationId, estimator, static, namespace, expander, config, itc.map(_.science.focus.value), calRole)
    )

  def gmosSouth[F[_]: Monad](
    observationId:  Observation.Id,
    estimator:      TimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
    namespace:      UUID,
    expander:       SmartGcalExpander[F, DynamicConfig.GmosSouth],
    config:         Config.GmosSouth,
    itc:            Either[OdbError, Spectroscopy],
    calRole:        Option[CalibrationRole],
    lastAcqReset:   Option[Timestamp]
  ): F[Either[OdbError, ExecutionConfigGenerator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]]] =
    val static = InitialConfigs.GmosSouthStatic
    instantiate(
      static,
      Acquisition.gmosSouth(observationId, estimator, static, namespace, config, itc.map(_.acquisition.focus.value), calRole, lastAcqReset),
      Science.gmosSouth(observationId, estimator, static, namespace, expander, config, itc.map(_.science.focus.value), calRole)
    )

end LongSlit