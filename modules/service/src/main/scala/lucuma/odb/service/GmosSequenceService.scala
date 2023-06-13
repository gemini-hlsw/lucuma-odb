// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.functor.*
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthStageMode
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthStageMode
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

trait GmosSequenceService[F[_]] {

  def insertGmosNorthDynamic(
    stepId:  Step.Id,
    dynamic: DynamicConfig.GmosNorth
  )(using Transaction[F]): F[Unit]

  def insertGmosNorthStatic(
    observationId: Observation.Id,
    visitId:       Option[Visit.Id],
    static:        StaticConfig.GmosNorth
  )(using Transaction[F]): F[Long]

  def insertGmosSouthDynamic(
    stepId:  Step.Id,
    dynamic: DynamicConfig.GmosSouth
  )(using Transaction[F]): F[Unit]

  def insertGmosSouthStatic(
    observationId: Observation.Id,
    visitId:       Option[Visit.Id],
    static:        StaticConfig.GmosSouth
  )(using Transaction[F]): F[Long]

}

object GmosSequenceService {

  def instantiate[F[_]: Concurrent](using Services[F]): GmosSequenceService[F] =

    new GmosSequenceService[F] {

      override def insertGmosNorthDynamic(
        stepId:  Step.Id,
        dynamic: DynamicConfig.GmosNorth
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.InsertGmosNorthDynamic)(stepId, dynamic).void

      override def insertGmosNorthStatic(
        observationId: Observation.Id,
        visitId:       Option[Visit.Id],
        static:        StaticConfig.GmosNorth
      )(using Transaction[F]): F[Long] =
        session.unique(Statements.InsertGmosNorthStatic)(
          observationId,
          visitId,
          static
        )

      override def insertGmosSouthDynamic(
        stepId:  Step.Id,
        dynamic: DynamicConfig.GmosSouth
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.InsertGmosSouthDynamic)(stepId, dynamic).void

      override def insertGmosSouthStatic(
        observationId: Observation.Id,
        visitId:       Option[Visit.Id],
        static:        StaticConfig.GmosSouth
      )(using Transaction[F]): F[Long] =
        session.unique(Statements.InsertGmosSouthStatic)(
          observationId,
          visitId,
          static
        )
   }

  object Statements {

    def insertGmosDynamic(site: String): Fragment[Void]  =
      sql"""
        INSERT INTO t_gmos_#${site}_dynamic (
          c_step_id,
          c_exposure_time,
          c_xbin,
          c_ybin,
          c_amp_count,
          c_amp_gain,
          c_amp_read_mode,
          c_dtax,
          c_roi,
          c_grating_disperser,
          c_grating_order,
          c_grating_wavelength,
          c_filter,
          c_fpu_custom_mask_filename,
          c_fpu_custom_mask_slit_width,
          c_fpu_builtin
        )
      """

    val InsertGmosNorthDynamic: Command[(Step.Id, DynamicConfig.GmosNorth)] =
      (insertGmosDynamic("north") ~> sql"SELECT $step_id, $gmos_north_dynamic").command

    val InsertGmosSouthDynamic: Command[(Step.Id, DynamicConfig.GmosSouth)] =
      (insertGmosDynamic("south") ~> sql"SELECT $step_id, $gmos_south_dynamic").command

    val InsertGmosNorthStatic: Query[(Observation.Id, Option[Visit.Id], StaticConfig.GmosNorth), Long] =
      sql"""
        INSERT INTO t_gmos_north_static (
          c_observation_id,
          c_visit_id,
          c_detector,
          c_mos_pre_imaging,
          c_stage_mode
        ) SELECT
          $observation_id,
          ${visit_id.opt},
          $gmos_north_static
        RETURNING c_static_id
      """.query(int8)

    val InsertGmosSouthStatic: Query[(Observation.Id, Option[Visit.Id], StaticConfig.GmosSouth), Long] =
      sql"""
        INSERT INTO t_gmos_south_static (
          c_observation_id,
          c_visit_id,
          c_detector,
          c_mos_pre_imaging,
          c_stage_mode
        ) SELECT
          $observation_id,
          ${visit_id.opt},
          $gmos_south_static
        RETURNING c_static_id
      """.query(int8)

  }
}