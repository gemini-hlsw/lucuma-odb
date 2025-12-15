// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package imaging

import cats.Monad
import cats.data.EitherT
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError

import java.util.UUID

object Imaging:

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
    config:         Config.GmosNorth,
    acquisitionItc: Either[OdbError, IntegrationTime],
    scienceItc:     Either[OdbError, Map[GmosNorthFilter, IntegrationTime]],
    lastAcqReset:   Option[Timestamp]
  ): F[Either[OdbError, ExecutionConfigGenerator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]]] =
    val static = config.variant.variantType match
      case VariantType.PreImaging => InitialConfigs.GmosNorthStatic.copy(mosPreImaging = MosPreImaging.IsMosPreImaging)
      case _                      => InitialConfigs.GmosNorthStatic

    instantiate(
      static,
      Acquisition.gmosNorth(observationId, estimator, static, namespace, config, acquisitionItc, lastAcqReset),
      Science.gmosNorth(observationId, estimator, static, namespace, config, scienceItc)
    )

  def gmosSouth[F[_]: Monad](
    observationId:  Observation.Id,
    estimator:      TimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
    namespace:      UUID,
    config:         Config.GmosSouth,
    acquisitionItc: Either[OdbError, IntegrationTime],
    scienceItc:     Either[OdbError, Map[GmosSouthFilter, IntegrationTime]],
    lastAcqReset:   Option[Timestamp]
  ): F[Either[OdbError, ExecutionConfigGenerator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]]] =
    val static = config.variant.variantType match
      case VariantType.PreImaging => InitialConfigs.GmosSouthStatic.copy(mosPreImaging = MosPreImaging.IsMosPreImaging)
      case _                      => InitialConfigs.GmosSouthStatic

    instantiate(
      static,
      Acquisition.gmosSouth(observationId, estimator, static, namespace, config, acquisitionItc, lastAcqReset),
      Science.gmosSouth(observationId, estimator, static, namespace, config, scienceItc)
    )