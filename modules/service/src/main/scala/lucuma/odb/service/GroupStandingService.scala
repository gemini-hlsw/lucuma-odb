// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.effect.Concurrent
import cats.effect.Ref
import cats.syntax.all.*
import lucuma.core.enums.GroupStandingStatus
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.group.GroupProgress
import lucuma.core.model.group.GroupStanding
import lucuma.core.util.Timestamp
import lucuma.odb.data.GroupTree
import lucuma.odb.logic.GroupProgressEvaluator
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int4
import skunk.codec.numeric.int8
import skunk.implicits.*

/**
 * Keeps the stored group standing (`t_group_standing`) up to date.  Edits mark
 * the top-level group trees they affect (`t_group_standing_mark`), and
 * `refresh` recalculates them.
 */
trait GroupStandingService[F[_]]:

  /**
   * Recalculates the standing of up to `max` top-level group trees that are
   * marked, due for a retry after a failure, or past the time when time
   * passing alone could change a standing.  Each tree is recalculated in its
   * own transaction.  Returns how many were recalculated.
   */
  def refresh(max: Int)(using ServiceAccess, NoTransaction[F]): F[Int]

object GroupStandingService:

  def instantiate[F[_]: Concurrent: Logger](using Services[F]): GroupStandingService[F] =
    new GroupStandingService[F]:

      override def refresh(max: Int)(using ServiceAccess, NoTransaction[F]): F[Int] =
        services
          .transactionally(session.execute(Statements.SelectDue)(max))
          .flatMap: roots =>
            roots.foldLeftM(0): (count, root) =>
              Ref.of[F, Option[Long]](None).flatMap: claimed =>
                services
                  .transactionally(recalculate(root, claimed))
                  .handleErrorWith: e =>
                    Logger[F].warn(e)(s"Failed to calculate the group standing of $root's tree") *>
                    claimed.get.flatMap(retry(root, _)).as(false)
                  .map(done => if done then count + 1 else count)

      // Only refreshers take the tree's advisory lock, one tree per
      // transaction, so it can't deadlock with anything.  It keeps two of them
      // from recalculating a tree at once, where the older snapshot could
      // commit last.  False when another refresher has the tree.
      //
      // The marks claimed are those committed before the claim, so reading
      // the tree afterwards sees their effects.  Marks committed later are
      // left for the next refresh.  `claimed` keeps the last mark claimed
      // for `retry`, since a failure rolls the claim back.
      private def recalculate(root: Group.Id, claimed: Ref[F, Option[Long]])(using Transaction[F]): F[Boolean] =
        session.unique(Statements.TryLock)(root).flatMap:
          case false => false.pure[F]
          case true  =>
            for
              ms  <- session.execute(Statements.ClaimMarks)(root)
              _   <- claimed.set(ms.maxOption)
              pid <- session.option(Statements.SelectTopLevelProgram)(root)
              _   <- pid.fold(session.execute(Statements.DeleteCalc)(root).void)(calculate(_, root))
            yield true

      // Clears the marks the failed attempt claimed, so that only a later
      // edit, or the retry time, recalculates the tree.
      private def retry(root: Group.Id, lastClaimed: Option[Long])(using NoTransaction[F]): F[Unit] =
        services
          .transactionally:
            session.unique(Statements.Lock)(root) *>
            lastClaimed.traverse_(m => session.execute(Statements.DeleteClaimed)(root, m)) *>
            session.execute(Statements.SetRetry)(root).void
          .handleErrorWith: e =>
            Logger[F].warn(e)(s"Failed to schedule a retry of the group standing of $root's tree")

      private def calculate(pid: Program.Id, root: Group.Id)(using Transaction[F]): F[Unit] =
        groupService.selectGroups(pid).flatMap: tree =>
          tree.findGroup(root) match
            case Some(b: GroupTree.Branch) =>
              for
                prog <- groupProgressService.selectProgress(List(GroupService.ProgramTree(pid, tree, b.allGroupIds)))
                now  <- timeService.transactionTime
                _    <- store(root, GroupProgressEvaluator.standings(List(b), prog), reevaluateAt(prog, now))
              yield ()
            case _                         =>
              session.execute(Statements.DeleteCalc)(root).void

      // The earliest time passing alone could change a standing: when an
      // ongoing group's window for its next element closes.
      private def reevaluateAt(prog: Map[Group.Id, GroupProgress], now: Timestamp): Option[Timestamp] =
        prog
          .values
          .collect:
            case GroupProgress.Ongoing(_, _, Some(w)) if w.end > now && w.end < Timestamp.Max => w.end
          .minOption

      private def store(
        root:         Group.Id,
        standings:    Map[Group.Id, GroupStanding],
        reevaluateAt: Option[Timestamp]
      ): F[Unit] =
        val rows = standings.toList.map: (gid, s) =>
          (gid, root, s.status, Statements.decidingGroup(s))
        session.execute(Statements.SetReady)(root, reevaluateAt) *>
        session.execute(Statements.DeleteOthers)(root, standings.keys.toList) *>
        session.execute(Statements.upsert(rows.length))(rows).void

  object Statements:

    def decidingGroup(s: GroupStanding): Option[Group.Id] =
      s match
        case GroupStanding.Viable           => None
        case GroupStanding.Complete(g)      => Some(g)
        case GroupStanding.Unschedulable(g) => Some(g)

    // The trees marked (oldest mark first), unless waiting to retry, then
    // those whose retry or re-evaluation time has passed.
    val SelectDue: Query[Int, Group.Id] =
      sql"""
        SELECT c_root_group_id
        FROM (
          SELECT m.c_root_group_id, MIN(m.c_mark_id) AS c_order
          FROM   t_group_standing_mark m
          LEFT JOIN t_group_standing_calc c ON c.c_group_id = m.c_root_group_id
          WHERE  c.c_retry_at IS NULL OR c.c_retry_at <= now()
          GROUP BY m.c_root_group_id
          UNION ALL
          SELECT c_group_id, NULL
          FROM   t_group_standing_calc
          WHERE  c_retry_at <= now()
             OR  (c_retry_at IS NULL AND c_reevaluate_at <= now())
        ) due
        GROUP BY c_root_group_id
        ORDER BY MIN(c_order) NULLS LAST
        LIMIT  $int4
      """.query(group_id)

    // The tree's lock, in a namespace of its own (V1294's group_open_hole
    // uses namespace 1).
    val TryLock: Query[Group.Id, Boolean] =
      sql"SELECT pg_try_advisory_xact_lock(2, hashtext($group_id))".query(bool)

    val Lock: Query[Group.Id, Unit] =
      sql"SELECT pg_advisory_xact_lock(2, hashtext($group_id))".query(void)

    val ClaimMarks: Query[Group.Id, Long] =
      sql"""
        DELETE FROM t_group_standing_mark
        WHERE c_root_group_id = $group_id
        RETURNING c_mark_id
      """.query(int8)

    val DeleteClaimed: Command[(Group.Id, Long)] =
      sql"""
        DELETE FROM t_group_standing_mark
        WHERE c_root_group_id = $group_id
          AND c_mark_id      <= $int8
      """.command

    // The program of a present top-level group.
    val SelectTopLevelProgram: Query[Group.Id, Program.Id] =
      sql"""
        SELECT c_program_id
        FROM   t_group
        WHERE  c_group_id  = $group_id
          AND  c_parent_id IS NULL
          AND  c_existence = 'present'
      """.query(program_id)

    // Also deletes the standings it wrote.
    val DeleteCalc: Command[Group.Id] =
      sql"""
        DELETE FROM t_group_standing_calc
        WHERE c_group_id = $group_id
      """.command

    // Standings of groups that have left the tree.
    val DeleteOthers: Command[(Group.Id, List[Group.Id])] =
      sql"""
        DELETE FROM t_group_standing
        WHERE c_root_group_id = $group_id
          AND NOT (c_group_id = ANY(${_gid[Group.Id]}))
      """.command

    def upsert(n: Int): Command[List[(Group.Id, Group.Id, GroupStandingStatus, Option[Group.Id])]] =
      sql"""
        INSERT INTO t_group_standing (
          c_group_id,
          c_root_group_id,
          c_standing,
          c_deciding_group_id
        ) VALUES ${(group_id *: group_id *: group_standing_status *: group_id.opt).values.list(n)}
        ON CONFLICT (c_group_id) DO UPDATE SET
          c_root_group_id     = EXCLUDED.c_root_group_id,
          c_standing          = EXCLUDED.c_standing,
          c_deciding_group_id = EXCLUDED.c_deciding_group_id
      """.command

    val SetReady: Command[(Group.Id, Option[Timestamp])] =
      sql"""
        INSERT INTO t_group_standing_calc (c_group_id, c_reevaluate_at)
        VALUES ($group_id, ${core_timestamp.opt})
        ON CONFLICT (c_group_id) DO UPDATE SET
          c_retry_at      = NULL,
          c_failure_count = 0,
          c_reevaluate_at = EXCLUDED.c_reevaluate_at
      """.command

    // Backs off like obscalc: 1, 2, 4 ... up to 32 minutes.
    val SetRetry: Command[Group.Id] =
      sql"""
        INSERT INTO t_group_standing_calc AS c (c_group_id, c_retry_at, c_failure_count)
        VALUES ($group_id, now() + interval '1 minute', 1)
        ON CONFLICT (c_group_id) DO UPDATE SET
          c_retry_at      = now() + interval '1 minute' * POWER(2, LEAST(c.c_failure_count, 5)),
          c_failure_count = c.c_failure_count + 1
      """.command
