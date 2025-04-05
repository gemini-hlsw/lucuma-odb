// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.longslit

import cats.Monad
import cats.data.EitherT
import cats.syntax.option.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthStageMode
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthStageMode
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.MissingParamSet

import java.util.UUID

object LongSlit:

  val GmosNorthStatic: StaticConfig.GmosNorth =
    StaticConfig.GmosNorth(
      GmosNorthStageMode.FollowXy,
      GmosNorthDetector.Hamamatsu,
      MosPreImaging.IsNotMosPreImaging,
      none
    )

  val GmosSouthStatic: StaticConfig.GmosSouth =
    StaticConfig.GmosSouth(
      GmosSouthStageMode.FollowXyz,
      GmosSouthDetector.Hamamatsu,
      MosPreImaging.IsNotMosPreImaging,
      none
    )

  private def instantiate[F[_]: Monad, S, D](
    static:      S,
    acquisition: Either[String, SequenceGenerator[D]],
    science:     F[Either[String, SequenceGenerator[D]]]
  ): F[Either[String, ExecutionConfigGenerator[S, D]]] =
    (for
      a <- EitherT.fromEither(acquisition)
      s <- EitherT(science)
    yield ExecutionConfigGenerator(static, a, s)).value

  def gmosNorth[F[_]: Monad](
    estimator:      TimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
    namespace:      UUID,
    expander:       SmartGcalExpander[F, DynamicConfig.GmosNorth],
    config:         Config.GmosNorth,
    acquisitionItc: Either[MissingParamSet, IntegrationTime],
    scienceItc:     Either[MissingParamSet, IntegrationTime],
    calRole:        Option[CalibrationRole],
    lastAcqReset:   Option[Timestamp]
  ): F[Either[String, ExecutionConfigGenerator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]]] =
    instantiate(
      GmosNorthStatic,
      Acquisition.gmosNorth(estimator, GmosNorthStatic, namespace, config, acquisitionItc, calRole, lastAcqReset),
      Science.gmosNorth(estimator, GmosNorthStatic, namespace, expander, config, scienceItc, calRole)
    )

  def gmosSouth[F[_]: Monad](
    estimator:      TimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
    namespace:      UUID,
    expander:       SmartGcalExpander[F, DynamicConfig.GmosSouth],
    config:         Config.GmosSouth,
    acquisitionItc: Either[MissingParamSet, IntegrationTime],
    scienceItc:     Either[MissingParamSet, IntegrationTime],
    calRole:        Option[CalibrationRole],
    lastAcqReset:   Option[Timestamp]
  ): F[Either[String, ExecutionConfigGenerator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]]] =
    instantiate(
      GmosSouthStatic,
      Acquisition.gmosSouth(estimator, GmosSouthStatic, namespace, config, acquisitionItc, calRole, lastAcqReset),
      Science.gmosSouth(estimator, GmosSouthStatic, namespace, expander, config, scienceItc, calRole)
    )

end LongSlit