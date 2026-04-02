// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.observing_mode_type
import lucuma.odb.util.Codecs.step_id
import lucuma.odb.util.Flamingos2Codecs.flamingos_2_dynamic
import lucuma.odb.util.Flamingos2Codecs.flamingos_2_static
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

trait Flamingos2SequenceService[F[_]]:

  def insertDynamic(
    stepId:  Step.Id,
    dynamic: Flamingos2DynamicConfig
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertStatic(
    observationId: Observation.Id,
    static:        Flamingos2StaticConfig
  )(using Transaction[F], Services.ServiceAccess): F[Option[Long]]

  def selectDynamicForStep(
    stepId: Step.Id
  )(using Transaction[F]): F[Option[Flamingos2DynamicConfig]]

  def selectStatic(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[Flamingos2StaticConfig]]

  def selectStaticOrDefault(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[Flamingos2StaticConfig]]

object Flamingos2SequenceService:

  def instantiate[F[_]: Concurrent](using Services[F]): Flamingos2SequenceService[F] =

    new Flamingos2SequenceService[F]:

      override def insertDynamic(
        stepId:  Step.Id,
        dynamic: Flamingos2DynamicConfig
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        session.execute(Statements.InsertDynamic)(stepId, dynamic).void

      private def defaultStatic(
        observationId: Observation.Id
      ): F[Option[Flamingos2StaticConfig]] =
        // We'll need something like the GmosSequenceService version when
        // F2 imaging is supported.
        session
          .option(Statements.SelectIsLongSlit)(observationId)
          .map(_.as(lucuma.odb.sequence.flamingos2.longslit.LongSlit.Static))

      override def selectStatic(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[Flamingos2StaticConfig]] =
        session.option(Statements.SelectStatic)(observationId)

      override def selectStaticOrDefault(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[Flamingos2StaticConfig]] =
        for
          s0 <- selectStatic(observationId)
          s1 <- s0.fold(defaultStatic(observationId))(_.some.pure[F])
        yield s1

      override def selectDynamicForStep(
        stepId: Step.Id
      )(using Transaction[F]): F[Option[Flamingos2DynamicConfig]] =
        session.option(Statements.SelectDynamicForStep)(stepId)

      override def insertStatic(
        observationId: Observation.Id,
        static:        Flamingos2StaticConfig
      )(using Transaction[F], Services.ServiceAccess): F[Option[Long]] =
        session.option(Statements.InsertStatic)(observationId, static)

  object Statements:

    val Flamingos2DynamicColumns: List[String] =
      List(
        "c_exposure_time",
        "c_disperser",
        "c_filter",
        "c_read_mode",
        "c_lyot_wheel",
        "c_fpu_custom_mask_filename",
        "c_fpu_custom_mask_slit_width",
        "c_fpu_builtin",
        "c_decker",
        "c_readout_mode",
        "c_reads"
      )

    val InsertDynamic: Command[(Step.Id, Flamingos2DynamicConfig)] =
      sql"""
        INSERT INTO t_flamingos_2_dynamic (
          c_step_id,
          #${encodeColumns(none, Flamingos2DynamicColumns)}
        ) SELECT
          $step_id,
          $flamingos_2_dynamic
      """.command

    val InsertStatic: Query[(Observation.Id, Flamingos2StaticConfig), Long] =
      sql"""
        INSERT INTO t_flamingos_2_static (
          c_observation_id,
          c_mos_pre_imaging,
          c_use_eoffsetting
        ) SELECT
          $observation_id,
          $flamingos_2_static
        ON CONFLICT DO NOTHING
        RETURNING c_static_id
      """.query(int8)

    val SelectStatic: Query[Observation.Id, Flamingos2StaticConfig] =
      sql"""
        SELECT
          c_mos_pre_imaging,
          c_use_eoffsetting
        FROM t_flamingos_2_static
        WHERE c_observation_id = $observation_id
      """.query(flamingos_2_static)

    val SelectIsLongSlit: Query[Observation.Id, Boolean] =
      sql"""
        SELECT EXISTS (
          SELECT 1
          FROM t_observation
          WHERE c_observation_id      = $observation_id
            AND c_observing_mode_type = $observing_mode_type
        )
      """.query(bool).contramap(o => (o, ObservingModeType.Flamingos2LongSlit))

    val SelectDynamicForStep: Query[Step.Id, Flamingos2DynamicConfig] =
      sql"""
        SELECT
          #${encodeColumns(none, Flamingos2DynamicColumns)}
        FROM t_flamingos_2_dynamic
        WHERE c_step_id = $step_id
      """.query(flamingos_2_dynamic)
