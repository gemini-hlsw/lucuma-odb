// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.step_id
import lucuma.odb.util.GnirsCodecs.gnirs_dynamic
import lucuma.odb.util.GnirsCodecs.gnirs_static
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

trait GnirsSequenceService[F[_]]:

  def insertStatic(
    observationId: Observation.Id,
    static:        GnirsStaticConfig
  )(using Transaction[F], Services.ServiceAccess): F[Option[Long]]

  def selectStatic(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[GnirsStaticConfig]]

  def selectStaticOrDefault(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[GnirsStaticConfig]]

object GnirsSequenceService:

  def instantiate[F[_]: Concurrent](using Services[F]): GnirsSequenceService[F] =
    new GnirsSequenceService[F]:

      override def insertStatic(
        observationId: Observation.Id,
        static:        GnirsStaticConfig
      )(using Transaction[F], Services.ServiceAccess): F[Option[Long]] =
        session.option(Statements.InsertStatic)(observationId, static)

      override def selectStatic(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[GnirsStaticConfig]] =
        session.option(Statements.SelectStaticByObservation)(observationId)

      override def selectStaticOrDefault(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[GnirsStaticConfig]] =
        for
          s0 <- selectStatic(observationId)
          s1 <- s0.fold(session.option(Statements.SelectStaticByMode)(observationId))(_.some.pure[F])
        yield s1

  object Statements:

    val GnirsDynamicColumns: List[String] =
      List(
        "c_exposure_time",
        "c_coadds",
        "c_central_wavelength",
        "c_filter",
        "c_decker",
        "c_fpu_slit",
        "c_fpu_other",
        "c_prism",
        "c_grating",
        "c_grating_wavelength",
        "c_camera",
        "c_focus_motor_steps",
        "c_read_mode"
      )

    val InsertDynamic: Command[(Step.Id, GnirsDynamicConfig)] =
      sql"""
        INSERT INTO t_gnirs_dynamic (
          c_step_id,
          #${encodeColumns(none, GnirsDynamicColumns)}
        ) SELECT
          $step_id,
          $gnirs_dynamic
      """.command

    val InsertStatic: Query[(Observation.Id, GnirsStaticConfig), Long] =
      sql"""
        INSERT INTO t_gnirs_static (
          c_observation_id,
          c_well_depth
        ) SELECT
          $observation_id,
          $gnirs_static
        ON CONFLICT DO NOTHING
        RETURNING c_static_id
      """.query(int8)

    val SelectStaticByObservation: Query[Observation.Id, GnirsStaticConfig] =
      sql"""
        SELECT
          c_well_depth
        FROM t_gnirs_static
        WHERE c_observation_id = $observation_id
      """.query(gnirs_static)

    val SelectStaticByMode: Query[Observation.Id, GnirsStaticConfig] =
      sql"""
        SELECT
          COALESCE(c_well_depth, c_well_depth_default)
        FROM v_gnirs_long_slit
        WHERE c_observation_id = $observation_id
      """.query(gnirs_static)
