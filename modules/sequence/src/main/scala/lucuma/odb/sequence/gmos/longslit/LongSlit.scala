// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.Timestamp
import lucuma.odb.data.Itc.Spectroscopy
import lucuma.odb.data.OdbError

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
    instantiate(
      GmosNorthStatic,
      Acquisition.gmosNorth(observationId, estimator, GmosNorthStatic, namespace, config, itc.map(_.acquisition.focus.value), calRole, lastAcqReset),
      Science.gmosNorth(observationId, estimator, GmosNorthStatic, namespace, expander, config, itc.map(_.science.focus.value), calRole)
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
    instantiate(
      GmosSouthStatic,
      Acquisition.gmosSouth(observationId, estimator, GmosSouthStatic, namespace, config, itc.map(_.acquisition.focus.value), calRole, lastAcqReset),
      Science.gmosSouth(observationId, estimator, GmosSouthStatic, namespace, expander, config, itc.map(_.science.focus.value), calRole)
    )

end LongSlit