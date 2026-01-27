// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package imaging

import cats.data.EitherT
import cats.effect.Sync
import fs2.Pure
import lucuma.core.enums.GmosImagingVariantType.PreImaging
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.odb.data.Itc.GmosNorthImaging
import lucuma.odb.data.Itc.GmosSouthImaging
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object Imaging:

  private def instantiate[F[_]: Sync, S, D](
    static:  S,
    science: F[Either[OdbError, SequenceGenerator[D]]]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, S, D]]] =
    EitherT(science)
      .map(s => StreamingExecutionConfig(static, SequenceGenerator.empty.generate, s.generate))
      .value

  def gmosNorth[F[_]: Sync](
    estimator:  TimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
    namespace:  UUID,
    config:     Config.GmosNorth,
    scienceItc: Either[OdbError, GmosNorthImaging]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, StaticConfig.GmosNorth, DynamicConfig.GmosNorth]]] =
    val static = config.variant.variantType match
      case PreImaging => InitialConfigs.GmosNorthStatic.copy(mosPreImaging = MosPreImaging.IsMosPreImaging)
      case _          => InitialConfigs.GmosNorthStatic

    instantiate(
      static,
      Science.gmosNorth(estimator, static, namespace, config, scienceItc)
    )

  def gmosSouth[F[_]: Sync](
    estimator:  TimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
    namespace:  UUID,
    config:     Config.GmosSouth,
    scienceItc: Either[OdbError, GmosSouthImaging]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, StaticConfig.GmosSouth, DynamicConfig.GmosSouth]]] =
    val static = config.variant.variantType match
      case PreImaging => InitialConfigs.GmosSouthStatic.copy(mosPreImaging = MosPreImaging.IsMosPreImaging)
      case _          => InitialConfigs.GmosSouthStatic

    instantiate(
      static,
      Science.gmosSouth(estimator, static, namespace, config, scienceItc)
    )