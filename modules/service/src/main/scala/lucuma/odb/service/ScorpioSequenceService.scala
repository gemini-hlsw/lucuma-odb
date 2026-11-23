// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.scorpio.ScorpioDynamicConfig
import lucuma.core.model.sequence.scorpio.ScorpioStaticConfig
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.ScorpioCodecs.*
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

trait ScorpioSequenceService[F[_]]:

  def insertDynamic(
    stepId:  Step.Id,
    dynamic: ScorpioDynamicConfig
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertStatic(
    observationId: Observation.Id,
    static:        ScorpioStaticConfig
  )(using Transaction[F], Services.ServiceAccess): F[Option[Long]]

  def selectStatic(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[ScorpioStaticConfig]]

object ScorpioSequenceService:

  def instantiate[F[_]: Concurrent](using Services[F]): ScorpioSequenceService[F] =

    new ScorpioSequenceService[F]:

      override def insertDynamic(
        stepId:  Step.Id,
        dynamic: ScorpioDynamicConfig
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        session.execute(Statements.InsertDynamic)(stepId, dynamic).void

      override def insertStatic(
        observationId: Observation.Id,
        static:        ScorpioStaticConfig
      )(using Transaction[F], Services.ServiceAccess): F[Option[Long]] =
        session.option(Statements.InsertStatic)(observationId, static)

      override def selectStatic(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[ScorpioStaticConfig]] =
        session.option(Statements.SelectStatic)(observationId)

  object Statements:

    val DynamicColumns: List[String] =
      List(
        "c_g_exposure_time",
        "c_g_exposure_count",
        "c_g_readout_mode",
        "c_r_exposure_time",
        "c_r_exposure_count",
        "c_r_readout_mode",
        "c_i_exposure_time",
        "c_i_exposure_count",
        "c_i_readout_mode",
        "c_z_exposure_time",
        "c_z_exposure_count",
        "c_z_readout_mode",
        "c_fpu",
        "c_disperser"
      )

    val InsertDynamic: Command[(Step.Id, ScorpioDynamicConfig)] =
      sql"""
        INSERT INTO t_scorpio_dynamic (
          c_step_id,
          #${encodeColumns(none, DynamicColumns)}
        )
        SELECT
          $step_id,
          $scorpio_dynamic
      """.command

    def insertDynamics(rows: List[(Step.Id, ScorpioDynamicConfig)]): Command[rows.type] =
      val enc = (step_id *: scorpio_dynamic).values.list(rows)
      sql"""
        INSERT INTO t_scorpio_dynamic (
          c_step_id,
          #${encodeColumns(none, DynamicColumns)}
        )
        VALUES $enc
      """.command

    val InsertStatic: Query[(Observation.Id, ScorpioStaticConfig), Long] =
      sql"""
        INSERT INTO t_scorpio_static (
          c_observation_id,
          c_channel_selection
        )
        SELECT
          $observation_id,
          $scorpio_static
        ON CONFLICT DO NOTHING
        RETURNING c_static_id
      """.query(int8)

    val SelectStatic: Query[Observation.Id, ScorpioStaticConfig] =
      sql"""
        SELECT
          c_channel_selection
        FROM t_scorpio_static
        WHERE c_observation_id = $observation_id
      """.query(scorpio_static)
