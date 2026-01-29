// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.option.*
import fs2.Stream
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

trait GmosSequenceService[F[_]] {

  def selectGmosNorthStepRecords(
    observationId: Observation.Id
  ): Stream[F, StepRecord[DynamicConfig.GmosNorth]]

  def selectGmosSouthStepRecords(
    observationId: Observation.Id
  ): Stream[F, StepRecord[DynamicConfig.GmosSouth]]

  def insertGmosNorthDynamic(
    stepId:  Step.Id,
    dynamic: DynamicConfig.GmosNorth
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  /**
   * Selects the static configuration corresponding to the given visit.
   */
  def selectGmosNorthStatic(
    visitId: Visit.Id
  )(using Transaction[F]): F[Option[StaticConfig.GmosNorth]]

  def selectLatestVisitGmosNorthStatic(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[StaticConfig.GmosNorth]]

  /**
   * Selects the dynamic configuration corresponding to the given step.
   */
  def selectGmosNorthDynamicForStep(
    stepId: Step.Id
  )(using Transaction[F]): F[Option[DynamicConfig.GmosNorth]]

  def insertGmosNorthStatic(
    observationId: Observation.Id,
    visitId:       Option[Visit.Id],
    static:        StaticConfig.GmosNorth
  )(using Transaction[F], Services.ServiceAccess): F[Long]

  def insertGmosSouthDynamic(
    stepId:  Step.Id,
    dynamic: DynamicConfig.GmosSouth
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  /**
   * Selects the static configuration corresponding to the given visit.
   */
  def selectGmosSouthStatic(
    visitId: Visit.Id
  )(using Transaction[F]): F[Option[StaticConfig.GmosSouth]]

  def selectLatestVisitGmosSouthStatic(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[StaticConfig.GmosSouth]]

  /**
   * Selects the dynamic configuration corresponding to the given step.
   */
  def selectGmosSouthDynamicForStep(
    stepId: Step.Id
  )(using Transaction[F]): F[Option[DynamicConfig.GmosSouth]]

  def insertGmosSouthStatic(
    observationId: Observation.Id,
    visitId:       Option[Visit.Id],
    static:        StaticConfig.GmosSouth
  )(using Transaction[F], Services.ServiceAccess): F[Long]

}

object GmosSequenceService {

  def instantiate[F[_]: Concurrent](using Services[F]): GmosSequenceService[F] =

    new GmosSequenceService[F] {

      override def insertGmosNorthDynamic(
        stepId:  Step.Id,
        dynamic: DynamicConfig.GmosNorth
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        session.execute(Statements.InsertGmosNorthDynamic)(stepId, dynamic).void

      override def selectGmosNorthStatic(
        visitId: Visit.Id
      )(using Transaction[F]): F[Option[StaticConfig.GmosNorth]] =
        session.option(Statements.SelectGmosNorthStatic)(visitId)

      override def selectLatestVisitGmosNorthStatic(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[StaticConfig.GmosNorth]] =
        session.option(Statements.selectLastestVisitStatic("north", gmos_north_static))(observationId)

      private def selectGmosStepRecords[A](
        observationId: Observation.Id,
        site:          String,
        decoderA:      Decoder[A]
      ): Stream[F, StepRecord[A]] =
        session.stream(
          SequenceService.Statements.selectStepRecord(
            s"t_gmos_${site}_dynamic",
            s"gmos$site",
            Statements.GmosDynamicColumns,
            decoderA
          )
        )(observationId, 1024)

      override def selectGmosNorthStepRecords(
        observationId: Observation.Id
      ): Stream[F, StepRecord[DynamicConfig.GmosNorth]] =
        selectGmosStepRecords(
          observationId,
          "north",
          gmos_north_dynamic
        )

      override def selectGmosSouthStepRecords(
        observationId: Observation.Id
      ): Stream[F, StepRecord[DynamicConfig.GmosSouth]] =
        selectGmosStepRecords(
          observationId,
          "south",
          gmos_south_dynamic
        )

      override def selectGmosNorthDynamicForStep(
        stepId: Step.Id
      )(using Transaction[F]): F[Option[DynamicConfig.GmosNorth]] =
        session.option(Statements.SelectGmosNorthDynamicForStep)(stepId)

      override def insertGmosNorthStatic(
        observationId: Observation.Id,
        visitId:       Option[Visit.Id],
        static:        StaticConfig.GmosNorth
      )(using Transaction[F], Services.ServiceAccess): F[Long] =
        session.unique(Statements.InsertGmosNorthStatic)(
          observationId,
          visitId,
          static
        )

      override def insertGmosSouthDynamic(
        stepId:  Step.Id,
        dynamic: DynamicConfig.GmosSouth
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        session.execute(Statements.InsertGmosSouthDynamic)(stepId, dynamic).void

      override def selectGmosSouthStatic(
        visitId: Visit.Id
      )(using Transaction[F]): F[Option[StaticConfig.GmosSouth]] =
        session.option(Statements.SelectGmosSouthStatic)(visitId)

      override def selectLatestVisitGmosSouthStatic(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[StaticConfig.GmosSouth]] =
        session.option(Statements.selectLastestVisitStatic("south", gmos_south_static))(observationId)

      override def selectGmosSouthDynamicForStep(
        stepId: Step.Id
      )(using Transaction[F]): F[Option[DynamicConfig.GmosSouth]] =
        session.option(Statements.SelectGmosSouthDynamicForStep)(stepId)

      override def insertGmosSouthStatic(
        observationId: Observation.Id,
        visitId:       Option[Visit.Id],
        static:        StaticConfig.GmosSouth
      )(using Transaction[F], Services.ServiceAccess): F[Long] =
        session.unique(Statements.InsertGmosSouthStatic)(
          observationId,
          visitId,
          static
        )
   }

  object Statements {

    val GmosDynamicColumns: List[String] =
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

    private def selectGmosDynamicStep[A](site: String, decoderA: Decoder[A]): Query[Step.Id, A] =
      sql"""
        SELECT
          #${encodeColumns(none, GmosDynamicColumns)}
        FROM t_gmos_#${site}_dynamic
        WHERE c_step_id = $step_id
      """.query(decoderA)

    val SelectGmosNorthDynamicForStep: Query[Step.Id, DynamicConfig.GmosNorth] =
      selectGmosDynamicStep("north", gmos_north_dynamic)

    val SelectGmosSouthDynamicForStep: Query[Step.Id, DynamicConfig.GmosSouth] =
      selectGmosDynamicStep("south", gmos_south_dynamic)

    private val GmosStaticColumns: List[String] =
      List(
        "c_detector",
        "c_mos_pre_imaging",
        "c_stage_mode"
      )

    def selectStatic[A](site: String, decoderA: Decoder[A]): Query[Visit.Id, A] =
      sql"""
        SELECT
          #${encodeColumns(none, GmosStaticColumns)}
        FROM t_gmos_#${site}_static
        WHERE c_visit_id = $visit_id
      """.query(decoderA)

    val SelectGmosNorthStatic: Query[Visit.Id, StaticConfig.GmosNorth] =
      selectStatic("north", gmos_north_static)

    val SelectGmosSouthStatic: Query[Visit.Id, StaticConfig.GmosSouth] =
      selectStatic("south", gmos_south_static)

    def selectLastestVisitStatic[A](site: String, decoderA: Decoder[A]): Query[Observation.Id, A] =
      sql"""
        SELECT
          #${encodeColumns("g".some, GmosStaticColumns)}
        FROM t_gmos_#${site}_static g
        JOIN t_visit v ON v.c_visit_id = g.c_visit_id
        WHERE v.c_observation_id = $observation_id
        ORDER BY v.c_created DESC
        LIMIT 1
      """.query(decoderA)

    def insertStatic[A](site: String, encoderA: Encoder[A]): Query[(Observation.Id, Option[Visit.Id], A), Long] =
      sql"""
        INSERT INTO t_gmos_#${site}_static (
          c_observation_id,
          c_visit_id,
          #${encodeColumns(none, GmosStaticColumns)}
        ) SELECT
          $observation_id,
          ${visit_id.opt},
          $encoderA
        RETURNING c_static_id
      """.query(int8)

    val InsertGmosNorthStatic: Query[(Observation.Id, Option[Visit.Id], StaticConfig.GmosNorth), Long] =
      insertStatic("north", gmos_north_static)

    val InsertGmosSouthStatic: Query[(Observation.Id, Option[Visit.Id], StaticConfig.GmosSouth), Long] =
      insertStatic("south", gmos_south_static)

  }
}
