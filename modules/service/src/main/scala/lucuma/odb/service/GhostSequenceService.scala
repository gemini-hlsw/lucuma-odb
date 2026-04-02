// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

trait GhostSequenceService[F[_]]:

  def insertDynamic(
    stepId:  Step.Id,
    dynamic: GhostDynamicConfig
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertStatic(
    observationId: Observation.Id,
    static:        GhostStaticConfig
  )(using Transaction[F], Services.ServiceAccess): F[Option[Long]]

  def selectStatic(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[GhostStaticConfig]]

  def selectStaticOrDefault(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[GhostStaticConfig]]

object GhostSequenceService:

  def instantiate[F[_]: Concurrent](using Services[F]): GhostSequenceService[F] =

    new GhostSequenceService[F]:

      override def insertDynamic(
        stepId:  Step.Id,
        dynamic: GhostDynamicConfig
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        session.execute(Statements.InsertDynamic)(stepId, dynamic).void

      override def insertStatic(
        observationId: Observation.Id,
        static:        GhostStaticConfig
      )(using Transaction[F], Services.ServiceAccess): F[Option[Long]] =
        session.option(Statements.InsertStatic)(observationId, static)

      override def selectStatic(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[GhostStaticConfig]] =
        session.option(Statements.SelectStatic)(observationId)

      private def defaultStatic: F[Option[GhostStaticConfig]] =
        // Placeholder
        GhostStaticConfig(GhostResolutionMode.Standard).some.pure[F]

      override def selectStaticOrDefault(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[GhostStaticConfig]] =
        for
          s0 <- selectStatic(observationId)
          s1 <- s0.fold(defaultStatic)(_.some.pure[F])
        yield s1

  object Statements:

    val DynamicColumns: List[String] =
      List(
        "c_blue_exposure_time",
        "c_blue_exposure_count",
        "c_blue_binning",
        "c_blue_read_mode",
        "c_red_exposure_time",
        "c_red_exposure_count",
        "c_red_binning",
        "c_red_read_mode",
        "c_ifu1_fiber_agitator",
        "c_ifu2_fiber_agitator"
      )

    val InsertDynamic: Command[(Step.Id, GhostDynamicConfig)] =
      sql"""
        INSERT INTO t_ghost_dynamic (
          c_step_id,
          #${encodeColumns(none, DynamicColumns)}
        )
        SELECT
          $step_id,
          $ghost_dynamic
      """.command

    val InsertStatic: Query[(Observation.Id, GhostStaticConfig), Long] =
      sql"""
        INSERT INTO t_ghost_static (
          c_observation_id,
          c_resolution_mode
        )
        SELECT
          $observation_id,
          $ghost_static
        ON CONFLICT DO NOTHING
        RETURNING c_static_id
      """.query(int8)

    val SelectStatic: Query[Observation.Id, GhostStaticConfig] =
      sql"""
        SELECT
          c_resolution_mode
        FROM t_ghost_static
        WHERE c_observation_id = $observation_id
      """.query(ghost_static)