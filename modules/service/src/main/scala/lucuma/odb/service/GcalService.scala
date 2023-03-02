// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Sync
import fs2.Stream
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric.*
import skunk.implicits.*

trait GcalService[F[_]] {

  def insert(
    gcal: Gcal
  ): F[Int]

  def select(
    gcal_id: Int
  ): F[Option[Gcal]]

}

object GcalService {

  def fromSession[F[_]: Sync](
    session: Session[F]
  ): GcalService[F] =

    new GcalService[F] {

      override def insert(g: Gcal): F[Int] =
        session.prepareR(Statements.insert.query(int4)).use { pq =>
          pq.unique(g)
        }

      override def select(gcal_id: Int): F[Option[Gcal]] =
        session.prepareR(Statements.select).use { pq =>
          pq.option(gcal_id)
        }

    }

  object Statements {

    val insert: Fragment[Gcal] =
      sql"""
        INSERT INTO t_gcal (
          c_continuum,
          c_ar_arc,
          c_cuar_arc,
          c_thar_arc,
          c_xe_arc,
          c_filter,
          c_diffuser,
          c_shutter
        )
        SELECT
          $step_config_gcal
        RETURNING c_gcal_id
      """

    val select: Query[Int, Gcal] =
      sql"""
        SELECT
          c_continuum,
          c_ar_arc,
          c_cuar_arc,
          c_thar_arc,
          c_xe_arc,
          c_filter,
          c_diffuser,
          c_shutter
        FROM t_gcal
        WHERE c_gcal_id = $int4
      """.query(step_config_gcal)
  }

}
