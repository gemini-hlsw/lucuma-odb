// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.option.*
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

  object Statements:

    val DynamicColumns: List[String] =
      List(
        "c_red_exposure_time",
        "c_red_exposure_count",
        "c_red_binning",
        "c_red_read_mode",
        "c_blue_exposure_time",
        "c_blue_exposure_count",
        "c_blue_binning",
        "c_blue_read_mode",
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
          c_program_id,
          c_observation_id,
          c_resolution_mode,
          c_ifu_mapping,
          c_ifu1_ra,
          c_ifu1_dec,
          c_ifu1_target_id,
          c_ifu2_ra,
          c_ifu2_dec,
          c_ifu2_target_id,
          c_slit_viewing_camera_exposure_time
        )
        SELECT
          o.c_program_id,
          $observation_id,
          $ghost_static
        FROM t_observation o
        WHERE o.c_observation_id = $observation_id
        ON CONFLICT DO NOTHING
        RETURNING c_static_id
      """.query(int8)
         .contramap { (o, g) => (o, g, o) }

    val SelectStatic: Query[Observation.Id, GhostStaticConfig] =
      sql"""
        SELECT
          c_resolution_mode,
          c_ifu_mapping,
          c_ifu1_ra,
          c_ifu1_dec,
          c_ifu1_target_id,
          c_ifu2_ra,
          c_ifu2_dec,
          c_ifu2_target_id,
          c_slit_viewing_camera_exposure_time
        FROM t_ghost_static
        WHERE c_observation_id = $observation_id
      """.query(ghost_static)