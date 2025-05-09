// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.f2.F2StaticConfig
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.visit_id
import lucuma.odb.util.Flamingos2Codecs.flamingos_2_static
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

trait Flamingos2SequenceService[F[_]]:
  def selectStatic(
    visitId: Visit.Id
  )(using Transaction[F]): F[Option[F2StaticConfig]]

  def insertStatic(
    observationId: Observation.Id,
    visitId:       Option[Visit.Id],
    static:        F2StaticConfig
  )(using Transaction[F], Services.ServiceAccess): F[Long]

object Flamingos2SequenceService:

  def instantiate[F[_]: Concurrent](using Services[F]): Flamingos2SequenceService[F] =

    new Flamingos2SequenceService[F]:

      override def selectStatic(
        visitId: Visit.Id
      )(using Transaction[F]): F[Option[F2StaticConfig]] =
        session.option(Statements.SelectStatic)(visitId)

      override def insertStatic(
        observationId: Observation.Id,
        visitId:       Option[Visit.Id],
        static:        F2StaticConfig
      )(using Transaction[F], Services.ServiceAccess): F[Long] =
        session.unique(Statements.InsertStatic)(
          observationId,
          visitId,
          static
        )

  object Statements:

    val SelectStatic: Query[Visit.Id, F2StaticConfig] =
      sql"""
        SELECT
          c_mos_pre_imaging,
          c_use_eoffsetting
        FROM t_flamingos_2_static
        WHERE c_visit_id = $visit_id
      """.query(flamingos_2_static)

    val InsertStatic: Query[(Observation.Id, Option[Visit.Id], F2StaticConfig), Long] =
      sql"""
        INSERT INTO t_flamingos_2_static (
          c_observation_id,
          c_visit_id,
          c_mos_pre_imaging,
          c_use_eoffsetting
        ) SELECT
          $observation_id,
          ${visit_id.opt},
          $flamingos_2_static
        RETURNING c_static_id
      """.query(int8)