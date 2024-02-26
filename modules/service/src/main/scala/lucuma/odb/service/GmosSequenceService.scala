// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import fs2.Stream
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

  def selectGmosNorthDynamic(
    observationId: Observation.Id
  )(using Transaction[F]): Stream[F, (Step.Id, DynamicConfig.GmosNorth)]

  def selectGmosNorthDynamicStep(
    stepId: Step.Id
  )(using Transaction[F]): F[Option[DynamicConfig.GmosNorth]]

  def insertGmosNorthStatic(
    observationId: Observation.Id,
    visitId:       Option[Visit.Id],
    static:        StaticConfig.GmosNorth
  )(using Transaction[F]): F[Long]

  def insertGmosSouthDynamic(
    stepId:  Step.Id,
    dynamic: DynamicConfig.GmosSouth
  )(using Transaction[F]): F[Unit]

  def selectGmosSouthDynamic(
    observationId: Observation.Id
  )(using Transaction[F]): Stream[F, (Step.Id, DynamicConfig.GmosSouth)]

  def selectGmosSouthDynamicStep(
    stepId: Step.Id
  )(using Transaction[F]): F[Option[DynamicConfig.GmosSouth]]

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

      override def selectGmosNorthDynamic(
        observationId: Observation.Id
      )(using Transaction[F]): Stream[F, (Step.Id, DynamicConfig.GmosNorth)] =
        session.stream(Statements.SelectGmosNorthDynamicForObs)(observationId, 1024)

      override def selectGmosNorthDynamicStep(
        stepId: Step.Id
      )(using Transaction[F]): F[Option[DynamicConfig.GmosNorth]] =
        session.option(Statements.SelectGmosNorthDynamicForStep)(stepId)

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

      override def selectGmosSouthDynamic(
        observationId: Observation.Id
      )(using Transaction[F]): Stream[F, (Step.Id, DynamicConfig.GmosSouth)] =
        session.stream(Statements.SelectGmosSouthDynamicForObs)(observationId, 1024)

      override def selectGmosSouthDynamicStep(
        stepId: Step.Id
      )(using Transaction[F]): F[Option[DynamicConfig.GmosSouth]] =
        session.option(Statements.SelectGmosSouthDynamicForStep)(stepId)

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

    def encodeColumns(prefix: Option[String], columns: List[String]): String =
      columns.map(c => s"${prefix.foldMap(_ + ".")}$c").intercalate(",\n")

    private val GmosDynamicColumns: List[String] =
      List(
        "c_exposure_time",
        "c_xbin",
        "c_ybin",
        "c_amp_count",
        "c_amp_gain",
        "c_amp_read_mode",
        "c_dtax",
        "c_roi",
        "c_grating_disperser",
        "c_grating_order",
        "c_grating_wavelength",
        "c_filter",
        "c_fpu_custom_mask_filename",
        "c_fpu_custom_mask_slit_width",
        "c_fpu_builtin"
      )

    private def insertGmosDynamic(site: String): Fragment[Void]  =
      sql"""
        INSERT INTO t_gmos_#${site}_dynamic (
          c_step_id,
          #${encodeColumns(none, GmosDynamicColumns)}
        )
      """

    val InsertGmosNorthDynamic: Command[(Step.Id, DynamicConfig.GmosNorth)] =
      (insertGmosDynamic("north") ~> sql"SELECT $step_id, $gmos_north_dynamic").command

    val InsertGmosSouthDynamic: Command[(Step.Id, DynamicConfig.GmosSouth)] =
      (insertGmosDynamic("south") ~> sql"SELECT $step_id, $gmos_south_dynamic").command

    private def selectGmosDynamic[A](site: String, decoderA: Decoder[A]): Query[Observation.Id, (Step.Id, A)] =
      (sql"""
        SELECT
          s.c_step_id,
          #${encodeColumns("c".some, GmosDynamicColumns)}
        FROM t_gmos_#${site}_dynamic c
        INNER JOIN t_step_record s ON s.c_step_id = c.c_step_id
        INNER JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE """ ~> sql"""a.c_observation_id = $observation_id"""
      ).query(step_id *: decoderA)

    private def selectGmosDynamicStep[A](site: String, decoderA: Decoder[A]): Query[Step.Id, A] =
      sql"""
        SELECT
          #${encodeColumns(none, GmosDynamicColumns)}
        FROM t_gmos_#${site}_dynamic
        WHERE c_step_id = $step_id
      """.query(decoderA)

    val SelectGmosNorthDynamicForObs: Query[Observation.Id, (Step.Id, DynamicConfig.GmosNorth)] =
      selectGmosDynamic("north", gmos_north_dynamic)

    val SelectGmosSouthDynamicForObs: Query[Observation.Id, (Step.Id, DynamicConfig.GmosSouth)] =
      selectGmosDynamic("south", gmos_south_dynamic)

    val SelectGmosNorthDynamicForStep: Query[Step.Id, DynamicConfig.GmosNorth] =
      selectGmosDynamicStep("north", gmos_north_dynamic)

    val SelectGmosSouthDynamicForStep: Query[Step.Id, DynamicConfig.GmosSouth] =
      selectGmosDynamicStep("south", gmos_south_dynamic)

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
