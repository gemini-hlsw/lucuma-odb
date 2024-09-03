// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.longslit

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthStageMode
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthStageMode
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.itc.IntegrationTime

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
    acquisition: SequenceGenerator[D],
    science:     F[Either[String, SequenceGenerator[D]]]
  ): F[Either[String, ExecutionConfigGenerator[S, D]]] =
    science.map(_.map(sci => ExecutionConfigGenerator(static, acquisition, sci)))

  def gmosNorth[F[_]: Monad](
    expander:       SmartGcalExpander[F, DynamicConfig.GmosNorth],
    config:         Config.GmosNorth,
    acquisitionItc: IntegrationTime,
    scienceItc:     IntegrationTime,
    calRole:        Option[CalibrationRole]
  ): F[Either[String, ExecutionConfigGenerator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]]] =
    instantiate(
      GmosNorthStatic,
      Acquisition.gmosNorth(config, acquisitionItc.exposureTime),
      Science.gmosNorth(expander, config, scienceItc, calRole)
    )

  def gmosSouth[F[_]: Monad](
    expander:       SmartGcalExpander[F, DynamicConfig.GmosSouth],
    config:         Config.GmosSouth,
    acquisitionItc: IntegrationTime,
    scienceItc:     IntegrationTime,
    calRole:        Option[CalibrationRole]
  ): F[Either[String, ExecutionConfigGenerator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]]] =
    instantiate(
      GmosSouthStatic,
      Acquisition.gmosSouth(config, acquisitionItc.exposureTime),
      Science.gmosSouth(expander, config, scienceItc, calRole)
    )

end LongSlit