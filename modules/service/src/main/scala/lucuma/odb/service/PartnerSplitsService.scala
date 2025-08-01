// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.core.model.Program
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.syntax.all.*

import Services.Syntax.*

private[service] trait PartnerSplitsService[F[_]] {

    def insertSplits(splits: Map[Partner, IntPercent], pid: Program.Id)(using Transaction[F], SuperUserAccess): F[Unit]

    def updateSplits(splits: Map[Partner, IntPercent], pid: Program.Id)(using Transaction[F], SuperUserAccess): F[Unit]

}

object PartnerSplitsService {

  /**
   * Construct a `PartnerSplitsService` using the specified `Session`. This service is intended for
  * indirect use by `ProposalService`.
   */
  def instantiate[F[_]: Concurrent](using Services[F]): PartnerSplitsService[F] =
    new PartnerSplitsService[F] {

      def insertSplits(splits: Map[Partner, IntPercent], pid: Program.Id)(using Transaction[F], SuperUserAccess): F[Unit] =
        NonEmptyList.fromList(splits.toList).traverse_ { lst =>
          session.prepareR(Statements.insertSplits(lst)).use(_.execute(pid, lst)).void
        }

      def updateSplits(splits: Map[Partner, IntPercent], pid: Program.Id)(using Transaction[F], SuperUserAccess): F[Unit] =
        for {
          _ <- deleteSplits(pid)
          _ <- insertSplits(splits, pid)
        } yield ()

      @annotation.nowarn("msg=unused implicit parameter")
      private def deleteSplits(pid: Program.Id)(using Transaction[F]): F[Unit] =
        session.prepareR(Statements.DeleteSplits).use(_.execute(pid)).void

    }

  private object Statements {

    def insertSplits(splits: NonEmptyList[(Partner, IntPercent)]): Command[(Program.Id, splits.type)] =
      sql"""
         INSERT INTO t_partner_split (c_program_id, c_partner, c_percent)
         VALUES ${(program_id *: partner *: int_percent).values.list(splits.size)}
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
