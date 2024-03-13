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

    def updateSplits(splits: Map[Tag, IntPercent], pid: Program.Id)(using Transaction[F]): F[Unit]

}

object PartnerSplitsService {

  /**
   * Construct a `PartnerSplitsService` using the specified `Session`. This service is intended for
  * indirect use by `ProposalService`.
   */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): PartnerSplitsService[F] =
    new PartnerSplitsService[F] {

      def insertSplits(splits: Map[Tag, IntPercent], pid: Program.Id)(using Transaction[F]): F[Unit] =
        session.prepareR(Statements.insertSplits(splits)).use(_.execute(pid, splits)).void

      def updateSplits(splits: Map[Tag, IntPercent], pid: Program.Id)(using Transaction[F]): F[Unit] = {

        // First delete all the splits for this program.
        val delete: F[Unit] =
          session.prepareR(Statements.DeleteSplits).use(_.execute(pid)).void

        for {
          _ <- delete
          _ <- insertSplits(splits, pid)
        } yield ()
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

    val DeleteSplits: Command[Program.Id] =
      sql"""
         DELETE FROM t_partner_split
         WHERE t_partner_split.c_program_id = $program_id
      """.command
  }
}
