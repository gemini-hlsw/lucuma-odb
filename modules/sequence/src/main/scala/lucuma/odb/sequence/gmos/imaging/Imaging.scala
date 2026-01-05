// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package imaging

import cats.Monad
import cats.data.EitherT
import lucuma.core.enums.GmosImagingVariantType.PreImaging
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.odb.data.Itc.GmosNorthImaging
import lucuma.odb.data.Itc.GmosSouthImaging
import lucuma.odb.data.OdbError

import java.util.UUID

object Imaging:

  private def instantiate[F[_]: Monad, S, D](
    static:  S,
    science: F[Either[OdbError, SequenceGenerator[D]]]
  ): F[Either[OdbError, ExecutionConfigGenerator[S, D]]] =
    EitherT(science)
      .map(s => ExecutionConfigGenerator(static, SequenceGenerator.empty, s))
      .value

  def gmosNorth[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
    namespace:     UUID,
    config:        Config.GmosNorth,
    scienceItc:    Either[OdbError, GmosNorthImaging]
  ): F[Either[OdbError, ExecutionConfigGenerator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]]] =
    val static = config.variant.variantType match
      case PreImaging => InitialConfigs.GmosNorthStatic.copy(mosPreImaging = MosPreImaging.IsMosPreImaging)
      case _          => InitialConfigs.GmosNorthStatic

    instantiate(
      static,
      Science.gmosNorth(observationId, estimator, static, namespace, config, scienceItc)
    )

  def gmosSouth[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
    namespace:     UUID,
    config:        Config.GmosSouth,
    scienceItc:    Either[OdbError, GmosSouthImaging]
  ): F[Either[OdbError, ExecutionConfigGenerator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]]] =
    val static = config.variant.variantType match
      case PreImaging => InitialConfigs.GmosSouthStatic.copy(mosPreImaging = MosPreImaging.IsMosPreImaging)
      case _          => InitialConfigs.GmosSouthStatic

    instantiate(
      static,
      Science.gmosSouth(observationId, estimator, static, namespace, config, scienceItc)
    )