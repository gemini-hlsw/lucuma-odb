// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.model.IntPercent
import lucuma.core.model.Program
import lucuma.odb.data.*
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.syntax.all.*

import Services.Syntax.*

private[service] trait PartnerSplitsService[F[_]] {

    def insertSplits(splits: Map[Tag, IntPercent], pid: Program.Id)(using Transaction[F]): F[Unit]

    def updateSplits(splits: Map[Tag, IntPercent])(using Transaction[F]): F[List[Program.Id]]

}

object PartnerSplitsService {

  /**
   * Construct a `PartnerSplitsService` using the specified `Session`. This service is intended for
  * indirect use by `ProgramService`, and we thus assume the presence of the `t_program_update` table.
   */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): PartnerSplitsService[F] =
    new PartnerSplitsService[F] {

      def insertSplits(splits: Map[Tag, IntPercent], pid: Program.Id)(using Transaction[F]): F[Unit] =
        session.prepareR(Statements.insertSplits(splits)).use(_.execute(pid, splits)).void

      def updateSplits(splits: Map[Tag, IntPercent])(using Transaction[F]): F[List[Program.Id]] = {

        // First delete all the splits for these programs.
        val a: F[List[Program.Id]] =
          session.prepareR(Statements.DeleteSplits).use(_.stream(Void, 1024).compile.toList)

        // Then insert the new ones
        val b: F[List[Program.Id]] = {
          val af = Statements.insertManySplits(splits)
          session.prepareR(af.fragment.query(program_id)).use(_.stream(af.argument, 1024).compile.toList)
        }

        // And combine the returned id lists (they should be the same though)
        (a, b).mapN((as, bs) => (as ++ bs).distinct)

      }

    }

  private object Statements {

    def insertSplits(splits: Map[Tag, IntPercent]): Command[(Program.Id, splits.type)] =
      sql"""
         INSERT INTO t_partner_split (c_program_id, c_partner, c_percent)
         VALUES ${(program_id *: tag *: int_percent).values.list(splits.size)}
      """.command
         .contramap {
          case (pid, splits) => splits.toList.map { case (t, p) => (pid, t, p) }
         }

    val DeleteSplits: Query[Void, Program.Id] =
      sql"""
         DELETE FROM t_partner_split
         USING t_program_update
         WHERE t_partner_split.c_program_id = t_program_update.c_program_id
         RETURNING t_partner_split.c_program_id
      """.query(program_id)

    def insertManySplits(splits: Map[Tag, IntPercent]) = {
      val splitsʹ = splits.toList
      sql"""
        INSERT INTO t_partner_split (c_program_id, c_partner, c_percent)
        SELECT c_program_id, partner, percent
        FROM (VALUES ${(tag ~ int_percent).values.list(splitsʹ)}) AS splits (partner, percent), t_program_update
        RETURNING c_program_id
      """.apply(splitsʹ)
    }

  }

}
