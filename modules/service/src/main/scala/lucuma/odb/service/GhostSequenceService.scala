// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.model.Observation
import lucuma.core.model.Visit
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
    visitId:       Option[Visit.Id],
    static:        GhostStaticConfig
  )(using Transaction[F], Services.ServiceAccess): F[Long]

  def selectStaticForVisit(
    visitId: Visit.Id
  )(using Transaction[F]): F[Option[GhostStaticConfig]]

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
        visitId:       Option[Visit.Id],
        static:        GhostStaticConfig
      )(using Transaction[F], Services.ServiceAccess): F[Long] =
        session.unique(Statements.InsertStatic)(observationId, visitId, static)

      override def selectStaticForVisit(
        visitId: Visit.Id
      )(using Transaction[F]): F[Option[GhostStaticConfig]] =
        session.option(Statements.SelectStaticForVisit)(visitId)

      override def selectStatic(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[GhostStaticConfig]] =
        session.option(Statements.SelectStatic)(observationId)


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

    val InsertStatic: Query[(Observation.Id, Option[Visit.Id], GhostStaticConfig), Long] =
      sql"""
        INSERT INTO t_ghost_static (
          c_observation_id,
          c_visit_id,
          c_resolution_mode
        )
        SELECT
          $observation_id,
          ${visit_id.opt},
          $ghost_static
        ON CONFLICT (c_observation_id, c_visit_id) DO UPDATE
          SET c_resolution_mode = EXCLUDED.c_resolution_mode
        RETURNING c_static_id
      """.query(int8)

    val SelectStaticForVisit: Query[Visit.Id, GhostStaticConfig] =
      sql"""
        SELECT
          c_resolution_mode
        FROM t_ghost_static
        WHERE c_visit_id = $visit_id
      """.query(ghost_static)

    val SelectStatic: Query[Observation.Id, GhostStaticConfig] =
      sql"""
        SELECT
          c_resolution_mode
        FROM t_ghost_static
        WHERE c_observation_id = $observation_id
          AND c_visit_id IS NULL
      """.query(ghost_static)