// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.option.*
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.step_id
import lucuma.odb.util.Codecs.visit_id
import lucuma.odb.util.Igrins2Codecs.igrins_2_dynamic
import lucuma.odb.util.Igrins2Codecs.igrins_2_static
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

trait Igrins2SequenceService[F[_]]:

  def insertStatic(
    observationId: Observation.Id,
    visitId:       Option[Visit.Id],
    static:        Igrins2StaticConfig
  )(using Transaction[F], Services.ServiceAccess): F[Long]

  def selectStaticForVisit(
    visitId: Visit.Id
  )(using Transaction[F]): F[Option[Igrins2StaticConfig]]

object Igrins2SequenceService:

  def instantiate[F[_]](using Services[F]): Igrins2SequenceService[F] =
    new Igrins2SequenceService[F]:

      override def insertStatic(
        observationId: Observation.Id,
        visitId:       Option[Visit.Id],
        static:        Igrins2StaticConfig
      )(using Transaction[F], Services.ServiceAccess): F[Long] =
        session.unique(Statements.InsertStatic)(observationId, visitId, static)

      override def selectStaticForVisit(
        visitId: Visit.Id
      )(using Transaction[F]): F[Option[Igrins2StaticConfig]] =
        session.option(Statements.SelectStaticByVisit)(visitId)

  object Statements:

    val Igrins2DynamicColumns: List[String] =
      List("c_exposure_time")

    val InsertDynamic: Command[(Step.Id, Igrins2DynamicConfig)] =
      sql"""
        INSERT INTO t_igrins_2_dynamic (
          c_step_id,
          #${encodeColumns(none, Igrins2DynamicColumns)}
        ) SELECT
          $step_id,
          $igrins_2_dynamic
      """.command

    val InsertStatic: Query[(Observation.Id, Option[Visit.Id], Igrins2StaticConfig), Long] =
      sql"""
        INSERT INTO t_igrins_2_static (
          c_observation_id,
          c_visit_id,
          c_save_svc_images,
          c_offset_mode
        ) SELECT
          $observation_id,
          ${visit_id.opt},
          $igrins_2_static
        RETURNING c_static_id
      """.query(int8)

    val SelectStaticByVisit: Query[Visit.Id, Igrins2StaticConfig] =
      sql"""
        SELECT
          c_save_svc_images,
          c_offset_mode
        FROM t_igrins_2_static
        WHERE c_visit_id = $visit_id
      """.query(igrins_2_static)
