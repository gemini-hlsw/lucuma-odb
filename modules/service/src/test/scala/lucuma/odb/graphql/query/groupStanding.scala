// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.ACursorOps
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.group_id
import lucuma.odb.util.Codecs.observation_id
import skunk.Command
import skunk.syntax.all.*

import scala.concurrent.duration.*

// The rule itself is tested in GroupProgressEvaluatorSuite.  These tests check
// that the standing is stored, kept up to date and exposed.
class groupStanding extends GroupTestSupport:

  def refresh: IO[Unit] =
    refreshGroupStandingsAs(serviceUser).void

  def standingJson(state: String, status: String, decidedBy: Option[Group.Id]): Json =
    json"""{ "calculationState": $state, "value": { "status": $status, "groupId": $decidedBy } }"""

  def viable(state: String = "READY"): Json =
    standingJson(state, "VIABLE", None)

  val StandingFields = "calculationState value { status groupId }"

  def observationStanding(oid: Observation.Id): IO[Json] =
    query(pi, s"""query { observation(observationId: "$oid") { groupStanding { $StandingFields } } }""")
      .map(_.hcursor.downFields("observation", "groupStanding").focus.getOrElse(Json.Null))

  def groupStanding(gid: Group.Id): IO[Json] =
    query(pi, s"""query { group(groupId: "$gid") { standing { $StandingFields } } }""")
      .map(_.hcursor.downFields("group", "standing").focus.getOrElse(Json.Null))

  def groupStatus(gid: Group.Id): IO[String] =
    query(pi, s"""query { group(groupId: "$gid") { progress { status } } }""")
      .map(_.hcursor.downFields("group", "progress", "status").require[String])

  def setMinimumRequired(gid: Group.Id, min: Int): IO[Unit] =
    query(pi, s"""mutation { updateGroups(input: { SET: { minimumRequired: $min }, WHERE: { id: { EQ: "$gid" } } }) { groups { id } } }""").void

  def deleteGroup(gid: Group.Id): IO[Unit] =
    query(pi, s"""mutation { updateGroups(input: { SET: { existence: DELETED }, WHERE: { id: { EQ: "$gid" } } }) { groups { id } } }""").void

  def calculatedTrees(gids: Group.Id*): IO[Set[Group.Id]] =
    withSession: s =>
      s.execute(sql"SELECT c_group_id FROM t_group_standing_calc".query(group_id)).map(_.toSet.intersect(gids.toSet))

  def reevaluateAt(gid: Group.Id): IO[Option[Timestamp]] =
    withSession: s =>
      s.unique(sql"SELECT c_reevaluate_at FROM t_group_standing_calc WHERE c_group_id = $group_id".query(core_timestamp.opt))(gid)

  def observationsWithStanding(pid: Program.Id, status: String): IO[Set[Observation.Id]] =
    query(
      pi,
      s"""
        query {
          observations(WHERE: { program: { id: { EQ: "$pid" } }, groupStanding: { status: { EQ: $status } } }) {
            matches { id }
          }
        }
      """
    ).map: js =>
      js.hcursor.downFields("observations", "matches").values.toList.flatten
        .map(_.hcursor.downField("id").require[Observation.Id])
        .toSet

  test("an ungrouped observation is viable, with nothing to calculate"):
    for
      p  <- createProgram
      o  <- newObservation(p)
      js <- observationStanding(o)
    yield assertEquals(js, viable())

  test("a new group is pending until calculated, then viable"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2))
      b  <- observationStanding(o1)
      _  <- refresh
      a  <- observationStanding(o1)
      ga <- groupStanding(g)
    yield
      assertEquals(b, viable("PENDING"))
      assertEquals(a, viable())
      assertEquals(ga, viable())

  test("an obscalc result that changes the execution state marks the tree pending"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), min = Some(1))
      _  <- completeObservation(o1)
      _  <- refresh   // whatever the completion itself marked
      _  <- runObscalcUpdate(p, o1)
      b  <- observationStanding(o2)
      _  <- refresh
      a  <- observationStanding(o2)
    yield
      assertEquals(b, standingJson("PENDING", "COMPLETE", Some(g)))
      assertEquals(a, standingJson("READY", "COMPLETE", Some(g)))

  test("an obscalc result with the same execution state leaves the tree ready"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), min = Some(1))
      _  <- completeObservation(o1)
      _  <- runObscalcUpdate(p, o1)
      _  <- refresh
      _  <- runObscalcUpdate(p, o1)   // completed again
      js <- observationStanding(o2)
    yield assertEquals(js, standingJson("READY", "COMPLETE", Some(g)))

  test("a group that is ongoing inside an unschedulable group stands as the outer group decides"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      o3 <- newObservation(p)
      o4 <- newObservation(p)
      // The outer group needs both subgroups, and the second can no longer
      // complete: it needs both of its observations, but one is inactive.
      g1 <- group(p, Nil)
      g2 <- group(p, List(o1, o2), parent = Some(g1))
      g3 <- group(p, List(o3, o4), min = Some(2), parent = Some(g1))
      _  <- setObservationWorkflowState(pi, o4, ObservationWorkflowState.Inactive)
      _  <- completeObservation(o1)
      _  <- refresh
      s2 <- groupStatus(g2)
      j2 <- groupStanding(g2)
      jo <- observationStanding(o2)
    yield
      assertEquals(s2, "ONGOING")
      assertEquals(j2, standingJson("READY", "UNSCHEDULABLE", Some(g1)))
      assertEquals(jo, standingJson("READY", "UNSCHEDULABLE", Some(g1)))

  test("editing a group marks its tree pending"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g1 <- group(p, Nil)
      g2 <- group(p, List(o1, o2), parent = Some(g1))
      _  <- refresh
      _  <- setMinimumRequired(g2, 1)
      j1 <- groupStanding(g1)
      jo <- observationStanding(o1)
    yield
      assertEquals(j1, viable("PENDING"))
      assertEquals(jo, viable("PENDING"))

  test("moving an observation marks both trees pending"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      ga <- group(p, List(o1))
      gb <- group(p, List(o2))
      _  <- refresh
      _  <- moveObservationAs(pi, o1, Some(gb))
      ja <- groupStanding(ga)
      jb <- groupStanding(gb)
    yield
      assertEquals(ja, viable("PENDING"))
      assertEquals(jb, viable("PENDING"))

  test("a top-level group moved into another tree is calculated with that tree"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      ga <- group(p, List(o1))
      gb <- group(p, List(o2))
      _  <- refresh
      _  <- moveGroupAs(pi, ga, Some(gb))
      _  <- refresh
      t  <- calculatedTrees(ga, gb)
      ja <- groupStanding(ga)
    yield
      assertEquals(t, Set(gb))
      assertEquals(ja, viable())

  test("a deleted group has no standing, and its tree is no longer calculated"):
    for
      p  <- createProgram
      g  <- group(p, Nil)
      _  <- refresh
      _  <- deleteGroup(g)
      _  <- refresh
      js <- groupStanding(g)
      t  <- calculatedTrees(g)
    yield
      assertEquals(js, Json.Null)
      assertEquals(t, Set.empty)

  val SetInactive: Command[Observation.Id] =
    sql"UPDATE t_observation SET c_workflow_user_state = 'inactive' WHERE c_observation_id = $observation_id".command

  test("edits in a tree don't wait on each other, and a mark committed after a refresh survives it"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2))
      _  <- refresh
      // While o1's edit is uncommitted, o2's edit in the same tree commits and
      // a refresh runs; neither waits for it, and the refresh can't see its
      // mark.
      _  <- withFreshSession: a =>
              a.transaction.use: _ =>
                a.execute(SetInactive)(o1) *>
                withFreshSession(_.execute(SetInactive)(o2)).timeout(15.seconds) *>
                refresh.timeout(15.seconds)
      b  <- groupStanding(g)
      _  <- refresh
      c  <- groupStanding(g)
    yield
      assertEquals(b, viable("PENDING"))
      assertEquals(c, viable())

  test("observations may be filtered by standing"):
    for
      p  <- createProgram
      o0 <- newObservation(p)
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      _  <- group(p, List(o1, o2), min = Some(1))
      _  <- completeObservation(o1)
      _  <- refresh
      vs <- observationsWithStanding(p, "VIABLE")
      cs <- observationsWithStanding(p, "COMPLETE")
    yield
      assertEquals(vs, Set(o0))
      assertEquals(cs, Set(o1, o2))

  def completionTime(oid: Observation.Id): IO[Timestamp] =
    query(pi, s"""query { observation(observationId: "$oid") { execution { timestamps { completionTime } } } }""")
      .map(_.hcursor.downFields("observation", "execution", "timestamps", "completionTime").require[Timestamp])

  // Moves an observation's execution two hours into the past, as if that much
  // time had gone by since.  Nothing marks the tree.
  def ageVisits(oid: Observation.Id): IO[Unit] =
    withSession: s =>
      s.execute(sql"""
        UPDATE t_visit
        SET    c_start = c_start - interval '2 hours',
               c_end   = c_end   - interval '2 hours'
        WHERE  c_observation_id = $observation_id
      """.command)(oid).void

  // As if the clock had reached the tree's re-evaluation time.
  def reachReevaluateAt(gid: Group.Id): IO[Unit] =
    withSession: s =>
      s.execute(sql"""
        UPDATE t_group_standing_calc
        SET    c_reevaluate_at = now() - interval '1 second'
        WHERE  c_group_id = $group_id
      """.command)(gid).void

  test("an ongoing group's window end is stored as its tree's re-evaluation time"):
    val maxI = 1.hrTimeSpan
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), maxI = Some(maxI))
      _  <- completeObservation(o1)
      _  <- refresh
      t  <- completionTime(o1)
      ra <- reevaluateAt(g)
      js <- observationStanding(o2)
    yield
      assertEquals(ra, Some(t +| maxI))
      assertEquals(js, viable())

  test("a tree is recalculated at its re-evaluation time, with no edit"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), maxI = Some(1.hrTimeSpan))
      _  <- completeObservation(o1)
      _  <- refresh
      // The window has now closed, but nothing says so yet.
      _  <- ageVisits(o1)
      _  <- refresh
      b  <- observationStanding(o2)
      _  <- reachReevaluateAt(g)
      _  <- refresh
      a  <- observationStanding(o2)
    yield
      assertEquals(b, viable())
      assertEquals(a, standingJson("READY", "UNSCHEDULABLE", Some(g)))
