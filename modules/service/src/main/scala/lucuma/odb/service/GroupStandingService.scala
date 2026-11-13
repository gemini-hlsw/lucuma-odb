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
import skunk.exception.PostgresErrorException
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
   * own transaction, so one that fails is retried later without holding up
   * the rest.  Stops early, leaving the rest for the next refresh, only once
   * the database session itself has failed.  Returns how many were
   * recalculated.
   */
  def refresh(max: Int)(using ServiceAccess, NoTransaction[F]): F[Int]

object GroupStandingService:

  def instantiate[F[_]: Concurrent: Logger](using Services[F]): GroupStandingService[F] =
    new GroupStandingService[F]:

      override def refresh(max: Int)(using ServiceAccess, NoTransaction[F]): F[Int] =
        services
          .transactionally(session.execute(Statements.SelectDue)(max))
          .flatMap: roots =>
            (roots, 0).tailRecM:
              case (Nil, count)          => count.asRight[(List[Group.Id], Int)].pure[F]
              case (root :: rest, count) =>
                refreshTree(root).flatMap:
                  case Some(done) =>
                    (rest, if done then count + 1 else count).asLeft[Int].pure[F]
                  case None       =>
                    Logger[F]
                      .warn(s"Stopping the group standing refresh, whose database session failed, with ${rest.length} trees left for the next one")
                      .as(count.asRight[(List[Group.Id], Int)])

      // Recalculates one tree: Some(whether it was recalculated) to go on with
      // the next, or None to stop.  A tree that fails is scheduled for a retry.
      // If even that fails, it may be the tree (its lock held too long, say),
      // or the session, which every later tree would find failed as well; it
      // is the session only if it no longer answers.  A deadlock with another
      // refresher isn't a failure of the tree, though: the rollback brings its
      // claimed marks back, so it is simply recalculated by the next refresh.
      private def refreshTree(root: Group.Id)(using ServiceAccess, NoTransaction[F]): F[Option[Boolean]] =
        Ref.of[F, Option[Long]](None).flatMap: claimed =>
          services
            .transactionally(recalculate(root, claimed))
            .map(_.some)
            .handleErrorWith:
              case e if isTransient(e) =>
                Logger[F].info(s"Recalculating the group standing of $root's tree conflicted with another refresh, so it is left for the next one: ${e.getMessage}")
                  .as(false.some)
              case e                   =>
                Logger[F].warn(e)(s"Failed to calculate the group standing of $root's tree") *>
                claimed.get.flatMap(retry(root, _)).flatMap:
                  case true  => false.some.pure[F]
                  case false => sessionAnswers.map(Option.when(_)(false))

      // A deadlock or serialization failure, which Postgres raises against one
      // of two transactions that conflict, and which trying again resolves.
      private def isTransient(e: Throwable): Boolean =
        e match
          case p: PostgresErrorException => Statements.TransientSqlStates.contains(p.code)
          case _                         => false

      private def sessionAnswers: F[Boolean] =
        session.unique(Statements.Ping).attempt.map(_.contains(true))

      // Only refreshers take the tree's advisory lock, one tree per
      // transaction, so the advisory lock itself can't deadlock.  It keeps two
      // of them from recalculating a tree at once, where the older snapshot
      // could commit last.  False when another refresher has the tree.
      // Refreshers of different trees may still write the same standing rows
      // (those of a group that has just moved between them), and can deadlock
      // on those: see `refreshTree`.
      //
      // The marks claimed are those committed before the claim, so reading
      // the tree afterwards sees their effects.  Marks committed later are
      // left for the next refresh.  `claimed` keeps the last mark claimed
      // for `retry`, since a failure rolls the claim back.
      //
      // A tree with no marks to claim may still be due, for its retry or
      // re-evaluation time, but if it no longer is, another refresher has
      // recalculated it since this one listed it as due, and it is skipped.
      // False then too.
      private def recalculate(root: Group.Id, claimed: Ref[F, Option[Long]])(using Transaction[F]): F[Boolean] =
        session.unique(Statements.TryLock)(root).flatMap:
          case false => false.pure[F]
          case true  =>
            for
              ms  <- session.execute(Statements.ClaimMarks)(root)
              _   <- claimed.set(ms.maxOption)
              due <- if ms.nonEmpty then true.pure[F] else session.unique(Statements.SelectDueByTime)(root)
              _   <- (
                       session.option(Statements.SelectTopLevelProgram)(root).flatMap:
                         _.fold(deleteTree(root))(calculate(_, root))
                     ).whenA(due)
            yield due

      // Clears the marks the failed attempt claimed, so that only a later
      // edit, or the retry time, recalculates the tree.  If another refresher
      // has the tree it is recalculating it anyway, so there is nothing to
      // schedule, and waiting for it would hold up the rest of the batch.
      // False if it failed.
      private def retry(root: Group.Id, lastClaimed: Option[Long])(using NoTransaction[F]): F[Boolean] =
        services
          .transactionally:
            session.unique(Statements.TryLock)(root).flatMap: locked =>
              (
                lastClaimed.traverse_(m => session.execute(Statements.DeleteClaimed)(root, m)) *>
                session.execute(Statements.SetRetry)(root) *>
                session.exec(Statements.insertPlaceholders(root))
              ).whenA(locked)
            .as(true)
          .handleErrorWith: e =>
            Logger[F].warn(e)(s"Failed to schedule a retry of the group standing of $root's tree").as(false)

      // Loads only the tree, not the rest of its program.  If the root has
      // just become a subgroup, it isn't found and its tree is removed.
      private def calculate(pid: Program.Id, root: Group.Id)(using Transaction[F]): F[Unit] =
        val inTree = Statements.inTree(root)
        groupService.selectGroups(pid, inTree, inTree).flatMap: tree =>
          tree.findGroup(root) match
            case Some(b: GroupTree.Branch) =>
              for
                prog <- groupProgressService.selectProgress(List(GroupService.ProgramTree(pid, tree, b.allGroupIds)))
                now  <- timeService.transactionTime
                ss    = GroupProgressEvaluator.standings(List(b), prog)
                _    <- store(root, ss, reevaluateAt(prog, ss, now))
              yield ()
            case _                         =>
              deleteTree(root)

      // Removes what was stored for a tree whose root is no longer a present
      // top-level group, handing its groups to the trees they are in now.
      private def deleteTree(root: Group.Id): F[Unit] =
        session.execute(Statements.DeleteOthers)(root, Nil, root) *>
        session.execute(Statements.DeleteCalc)(root).void

      // The earliest time passing alone could change a standing: when an
      // ongoing group's window for its next element closes.  A window closing
      // just now is kept, since a deadline may be violated only once passed.
      // Only viable groups count: one inside a complete or unschedulable group
      // stands as that group decides, which time passing can't undo.
      private def reevaluateAt(
        prog:      Map[Group.Id, GroupProgress],
        standings: Map[Group.Id, GroupStanding],
        now:       Timestamp
      ): Option[Timestamp] =
        prog
          .toList
          .collect:
            case (gid, GroupProgress.Ongoing(_, _, Some(w), _))
              if standings.get(gid).contains(GroupStanding.Viable) && w.end >= now && w.end < Timestamp.Max => w.end
          .minOption

      private def store(
        root:         Group.Id,
        standings:    Map[Group.Id, GroupStanding],
        reevaluateAt: Option[Timestamp]
      ): F[Unit] =
        // In group order, so that two refreshers upserting the same rows lock
        // them in the same order.
        val rows = standings.toList.sortBy(_._1).map: (gid, s) =>
          (gid, root, s.status, Statements.decidingGroup(s))
        session.execute(Statements.SetReady)(root, reevaluateAt) *>
        session.execute(Statements.DeleteOthers)(root, standings.keys.toList, root) *>
        session.execute(Statements.upsert(rows.length))(rows).void

  object Statements:

    // deadlock_detected and serialization_failure.
    val TransientSqlStates: Set[String] = Set("40P01", "40001")

    def decidingGroup(s: GroupStanding): Option[Group.Id] =
      s match
        case GroupStanding.Viable           => None
        case GroupStanding.Complete(g)      => Some(g)
        case GroupStanding.Unschedulable(g) => Some(g)

    // The trees whose retry or re-evaluation time has passed, which are few
    // and can't wait behind a stream of edits, then the trees marked (oldest
    // mark first), unless waiting to retry.
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
        ORDER BY bool_or(c_order IS NULL) DESC, MIN(c_order)
        LIMIT  $int4
      """.query(group_id)

    // The tree's lock, in a namespace of its own (V1294's group_open_hole
    // uses namespace 1).
    val TryLock: Query[Group.Id, Boolean] =
      sql"SELECT pg_try_advisory_xact_lock(2, hashtext($group_id))".query(bool)

    val Ping: Query[Void, Boolean] =
      sql"SELECT true".query(bool)

    // Whether the tree is due for its retry or re-evaluation time, as
    // SelectDue lists it without marks.
    val SelectDueByTime: Query[Group.Id, Boolean] =
      sql"""
        SELECT EXISTS (
          SELECT 1
          FROM   t_group_standing_calc
          WHERE  c_group_id = $group_id
            AND  (c_retry_at <= now() OR (c_retry_at IS NULL AND c_reevaluate_at <= now()))
        )
      """.query(bool)

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

    // A filter for the present groups, or observations, in the tree under
    // `root`: those whose group is the root or one of its subgroups.  Like
    // group_root, it stops at a group already seen should the groups be in a
    // cycle for the moment.
    def inTree(root: Group.Id): AppliedFragment =
      sql"""
        c_existence = 'present' AND c_group_id IN (
          WITH RECURSIVE down AS (
            SELECT c_group_id FROM t_group WHERE c_group_id = $group_id
            UNION
            SELECT g.c_group_id FROM t_group g JOIN down ON g.c_parent_id = down.c_group_id
          )
          SELECT c_group_id FROM down
        )
      """.apply(root)

    // A viable standing, the default shown for a group never calculated, for
    // each present group in the tree under `root` that has no standing yet,
    // when `root` is a present top-level group.  Written when a retry is
    // scheduled, so that the views find the tree's calculation, and read
    // retry rather than never calculated, until a calculation succeeds and
    // replaces them.
    def insertPlaceholders(root: Group.Id): AppliedFragment =
      sql"""
        INSERT INTO t_group_standing (c_group_id, c_root_group_id, c_standing, c_deciding_group_id)
        SELECT c_group_id, $group_id, 'viable' :: e_group_standing, NULL
        FROM   t_group
        WHERE  EXISTS (
                 SELECT 1 FROM t_group r
                 WHERE  r.c_group_id = $group_id AND r.c_parent_id IS NULL AND r.c_existence = 'present'
               )
          AND  """.apply((root, root)) |+| inTree(root) |+|
      sql" ON CONFLICT (c_group_id) DO NOTHING".apply(Void)

    // The program of a present top-level group.
    val SelectTopLevelProgram: Query[Group.Id, Program.Id] =
      sql"""
        SELECT c_program_id
        FROM   t_group
        WHERE  c_group_id  = $group_id
          AND  c_parent_id IS NULL
          AND  c_existence = 'present'
      """.query(program_id)

    // Also deletes, by cascade, any standings left with it.
    val DeleteCalc: Command[Group.Id] =
      sql"""
        DELETE FROM t_group_standing_calc
        WHERE c_group_id = $group_id
      """.command

    // Deletes the standings stored with the tree for groups other than those
    // given, which have left it, and marks the trees those groups are in now.
    //
    // With refreshers working different trees at once, the refresh of a tree
    // a group has left may read it before the move and write the group's
    // standing back under the old tree, after the new tree's refresh has
    // stored it.  The old tree, marked by the move, then deletes it here.
    // Marking the group's tree now has that tree store it again.  (A deleted
    // group needs nothing more, and a group back in this tree after its read
    // was marked here by the move.)
    val DeleteOthers: Command[(Group.Id, List[Group.Id], Group.Id)] =
      sql"""
        WITH removed AS (
          DELETE FROM t_group_standing
          WHERE c_root_group_id = $group_id
            AND NOT (c_group_id = ANY(${_gid[Group.Id]}))
          RETURNING c_group_id
        )
        INSERT INTO t_group_standing_mark (c_root_group_id)
        SELECT DISTINCT r.c_root
        FROM removed d
        JOIN t_group g ON g.c_group_id = d.c_group_id AND g.c_existence = 'present'
        CROSS JOIN LATERAL (SELECT group_root(g.c_group_id) AS c_root) r
        WHERE r.c_root IS NOT NULL
          AND r.c_root <> $group_id
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
