// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ToOActivation
import lucuma.core.model.GuestRole
import lucuma.core.model.IntPercent
import lucuma.core.model.Program
import lucuma.core.model.ServiceRole
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.User
import lucuma.odb.data._
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.graphql.input.ProposalClassInput
import lucuma.odb.graphql.input.ProposalInput
import lucuma.odb.util.Codecs._
import natchez.Trace
import skunk._
import skunk.codec.all._
import skunk.syntax.all._

private[service] trait PartnerSplitsService[F[_]] {

    def insertSplits(splits: Map[Tag, IntPercent], pid: Program.Id, xa: Transaction[F]): F[Unit]

    def updateSplits(splits: Map[Tag, IntPercent], whichProgramIds: AppliedFragment, xa: Transaction[F]): F[List[Program.Id]]

}

object PartnerSplitsService {

  /**
   * Construct a `PartnerSplitsService` using the specified `Session`. This service is intended for
  * indirect use by `ProgramService`, which is in charge of access control (which is unchecked here).
   */
  def fromSession[F[_]: Concurrent: Trace](s: Session[F]): PartnerSplitsService[F] =
    new PartnerSplitsService[F] {

      def insertSplits(splits: Map[Tag, IntPercent], pid: Program.Id, xa: Transaction[F]): F[Unit] =
        s.prepare(Statements.insertSplits(splits)).use(_.execute(pid ~ splits)).void

      def updateSplits(splits: Map[Tag, IntPercent], whichProgramIds: AppliedFragment, xa: Transaction[F]): F[List[Program.Id]] = {

        // First delete all the splits for these programs.
        val a: F[List[Program.Id]] = {
          val af = Statements.deleteSplits(whichProgramIds)
          s.prepare(af.fragment.query(program_id)).use(_.stream(af.argument, 1024).compile.toList)
        }

        // Then insert the new ones
        val b: F[List[Program.Id]] = {
          val af = Statements.insertManySplits(splits, whichProgramIds)
          s.prepare(af.fragment.query(program_id)).use(_.stream(af.argument, 1024).compile.toList)
        }

        // And combine the returned id lists (they should be the same though)
        (a, b).mapN((as, bs) => (as ++ bs).distinct)

      }

    }

  private object Statements {

    def insertSplits(splits: Map[Tag, IntPercent]): Command[Program.Id ~ splits.type] =
      sql"""
         INSERT INTO t_partner_split (c_program_id, c_partner, c_percent)
         VALUES ${(program_id ~ tag ~ int_percent).values.list(splits.size)}
      """.command
         .contramap {
          case pid ~ splits => splits.toList.map { case (t, p) => pid ~ t ~ p }
         }

    def deleteSplits(whichProgramIds: AppliedFragment): AppliedFragment =
      void"""
         DELETE FROM t_partner_split
         WHERE c_program_id IN (""" |+| whichProgramIds |+| void""")
         RETURNING c_program_id
      """

    def insertManySplits(splits: Map[Tag, IntPercent], whichProgramIds: AppliedFragment) = {
      val splitsʹ = splits.toList
      sql"""
        INSERT INTO t_partner_split (c_program_id, c_partner, c_percent)
        SELECT pid, partner, percent
        FROM (VALUES ${(tag ~ int_percent).values.list(splitsʹ)}) AS splits (partner, percent),
      """.apply(splitsʹ) |+| void"(" |+| whichProgramIds |+| void""") AS pids (pid)
        RETURNING c_program_id
      """
    }

  }

}