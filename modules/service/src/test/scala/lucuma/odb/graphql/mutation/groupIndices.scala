// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*
import skunk.exception.PostgresErrorException
import skunk.syntax.all.*

/**
 * The elements of a group -- child groups and observations, which live in separate tables --
 * must together be numbered 0..n-1 with no gaps and no repeats. The API cannot express a
 * violation, so these tests write the bad state directly and check what the database does with
 * it. Before V1294 a duplicated index was invisible to `group_verify_indices`, which combined
 * the two tables with UNION and so never saw the second element sharing a slot.
 */
class groupIndices extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi)

  /** A program whose top level holds three observations, numbered 0, 1, 2. */
  private def threeAtTopLevel: IO[(Program.Id, List[Observation.Id])] =
    for
      pid <- createProgramAs(pi)
      o0  <- createObservationInGroupAs(pi, pid, none)
      o1  <- createObservationInGroupAs(pi, pid, none)
      o2  <- createObservationInGroupAs(pi, pid, none)
    yield (pid, List(o0, o1, o2))

  private def setGroupIndex(oid: Observation.Id, index: Short): IO[Unit] =
    withFreshSession: s =>
      s.execute(
        sql"update t_observation set c_group_index = $int2 where c_observation_id = $observation_id".command
      )((index, oid)).void

  /**
   * Corrupt the numbering with constraints deferred, optionally repairing before commit. The
   * verification triggers are DEFERRABLE, so the bad state is legal mid-transaction and only
   * has to be gone by the time we commit.
   */
  private def deferred(pid: Program.Id, oid: Observation.Id, index: Short, repair: Boolean): IO[Unit] =
    withFreshSession: s =>
      s.transaction.use: _ =>
        s.execute(sql"set constraints all deferred".command) >>
        s.execute(
          sql"update t_observation set c_group_index = $int2 where c_observation_id = $observation_id".command
        )((index, oid)) >>
        s.execute(sql"call group_compact($program_id, null)".command)(pid).void.whenA(repair)

  private def indicesAtTopLevel(pid: Program.Id): IO[List[Int]] =
    withSession: s =>
      s.execute(
        sql"""
          select c_index from v_group_element
          where c_program_id = $program_id and c_group_id is null
          order by c_index
        """.query(int2)
      )(pid).map(_.map(_.toInt))

  // This is the case that went undetected in production. Collapsing the duplicate leaves
  // {0, 1}, which starts at zero and is consecutive, so the pre-V1294 check passed it. Putting
  // the duplicate in the middle instead (0, 0, 2) leaves a visible gap and was always caught,
  // which is why the bug survived so long.
  test("a duplicate index at the end of a group is rejected"):
    for
      (_, oids) <- threeAtTopLevel
      e         <- setGroupIndex(oids(2), 1).intercept[PostgresErrorException]
    yield assert(
      e.message.startsWith("Duplicate index detected"),
      s"unexpected error: ${e.message}"
    )

  test("a duplicate index in the middle of a group is rejected"):
    for
      (_, oids) <- threeAtTopLevel
      e         <- setGroupIndex(oids(1), 0).intercept[PostgresErrorException]
    yield assert(
      e.message.startsWith("Duplicate index detected"),
      s"unexpected error: ${e.message}"
    )

  test("a hole in a group is rejected"):
    for
      (_, oids) <- threeAtTopLevel
      e         <- setGroupIndex(oids(1), 7).intercept[PostgresErrorException]
    yield assert(
      e.message.startsWith("Index discontinuity detected"),
      s"unexpected error: ${e.message}"
    )

  test("a duplicate index is still rejected when the check is deferred to commit"):
    for
      (pid, oids) <- threeAtTopLevel
      e           <- deferred(pid, oids(2), 1, repair = false).intercept[PostgresErrorException]
      ix          <- indicesAtTopLevel(pid)
    yield
      assert(e.message.startsWith("Duplicate index detected"), s"unexpected error: ${e.message}")
      assertEquals(ix, List(0, 1, 2))  // rolled back

  // group_open_hole reads max(c_index) + 1 and hands it back for the caller to insert at, so
  // without serialization two concurrent creates in one program are handed the same slot. This
  // is the mechanism behind the duplicates found in production: separate calibration
  // recalculation transactions, each computing the next index from its own snapshot.
  test("concurrent creates in one program get distinct indices"):
    val N = 6
    for
      pid  <- createProgramAs(pi)
      _    <- List.fill(N)(()).parTraverse(_ => createObservationInGroupAs(pi, pid, none))
      ix   <- indicesAtTopLevel(pid)
    yield assertEquals(ix, List.range(0, N))

  /** Corrupt with constraints deferred, then compact before commit. */
  private def corruptThenCompact(pid: Program.Id)(corrupt: skunk.Session[IO] => IO[Unit]): IO[Unit] =
    withFreshSession: s =>
      s.transaction.use: _ =>
        s.execute(sql"set constraints all deferred".command) >>
        corrupt(s) >>
        s.execute(sql"call group_compact($program_id, null)".command)(pid).void

  // Compaction renumbers child groups through a data-modifying CTE whose output the enclosing
  // statement never reads. Postgres runs such a CTE to completion regardless of that, and this
  // is the test that holds it down: were the group update skipped, the two groups would still
  // share index 0 and the deferred check would reject the commit.
  test("child groups sharing an index are renumbered too"):
    for
      pid <- createProgramAs(pi)
      g0  <- createGroupAs(pi, pid)
      g1  <- createGroupAs(pi, pid)
      _   <- corruptThenCompact(pid): s =>
               s.execute(
                 sql"update t_group set c_parent_index = 0 where c_group_id = $group_id".command
               )(g1).void
      ix  <- indicesAtTopLevel(pid)
      els <- groupElementsAs(pi, pid, none)
    yield
      assertEquals(ix, List(0, 1))
      assertEquals(els.toSet, Set(g0.asLeft[Observation.Id], g1.asLeft[Observation.Id]))

  // The production shape in p-10d0: a user group and a system group sharing a slot. Both
  // tables are renumbered from one snapshot in a single statement, so neither update can see
  // the other's writes and perturb the numbering.
  test("a group and an observation sharing an index are renumbered together"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      oid <- createObservationInGroupAs(pi, pid, none)
      _   <- corruptThenCompact(pid): s =>
               s.execute(
                 sql"update t_observation set c_group_index = 0 where c_observation_id = $observation_id".command
               )(oid).void
      ix  <- indicesAtTopLevel(pid)
      els <- groupElementsAs(pi, pid, none)
    yield
      assertEquals(ix, List(0, 1))
      assertEquals(els.toSet, Set(gid.asLeft[Observation.Id], oid.asRight[Group.Id]))

  test("group_compact renumbers a duplicated group so the commit succeeds"):
    for
      (pid, oids) <- threeAtTopLevel
      _           <- deferred(pid, oids(2), 1, repair = true)
      ix          <- indicesAtTopLevel(pid)
      elems       <- groupElementsAs(pi, pid, none)
    yield
      assertEquals(ix, List(0, 1, 2))
      // All three elements are still addressable: a duplicated index makes two elements share
      // the GroupElement key (program:parent:index) and one of them disappears from the API.
      assertEquals(elems.size, 3)
      assertEquals(elems.toSet, oids.map(_.asRight[Group.Id]).toSet)

}
