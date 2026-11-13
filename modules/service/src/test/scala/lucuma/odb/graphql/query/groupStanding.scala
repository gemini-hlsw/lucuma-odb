// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.foldable.*
import cats.syntax.order.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SlewStage
import lucuma.core.enums.StepStage
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.ACursorOps
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.group_id
import lucuma.odb.util.Codecs.observation_id
import org.typelevel.log4cats.Logger
import skunk.Command
import skunk.codec.boolean.bool
import skunk.codec.numeric.int4
import skunk.codec.numeric.int8
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

  // As if a refresh of `root`'s tree that read it before `gids` moved out had
  // written their standings back under it after their new tree stored them,
  // and the mark the move left on `root` had survived that refresh.
  def writeStale(root: Group.Id, gids: Group.Id*): IO[Unit] =
    withSession: s =>
      for
        _ <- s.execute(sql"INSERT INTO t_group_standing_calc (c_group_id) VALUES ($group_id) ON CONFLICT DO NOTHING".command)(root)
        _ <- gids.toList.traverse_(gid => s.execute(sql"UPDATE t_group_standing SET c_root_group_id = $group_id WHERE c_group_id = $group_id".command)((root, gid)))
        _ <- s.execute(sql"INSERT INTO t_group_standing_mark (c_root_group_id) VALUES ($group_id)".command)(root)
      yield ()

  test("a moved group's standing, written back stale under its old tree, is stored again by its new tree"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      ga <- group(p, Nil)
      x  <- group(p, List(o1), parent = Some(ga))
      gb <- group(p, List(o2))
      _  <- refresh
      _  <- moveGroupAs(pi, x, Some(gb))
      _  <- refresh
      _  <- writeStale(ga, x)
      _  <- refresh   // ga's tree drops x's standing
      b  <- groupStanding(x)
      _  <- refresh   // gb's tree stores it again
      a  <- groupStanding(x)
    yield
      assertEquals(b, viable("PENDING"))
      assertEquals(a, viable())

  test("a top-level group moved into another tree, written back stale as a tree of its own, is stored again by its new tree"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, Nil)
      s  <- group(p, List(o1), parent = Some(g))
      gb <- group(p, List(o2))
      _  <- refresh
      _  <- moveGroupAs(pi, g, Some(gb))
      _  <- refresh
      _  <- writeStale(g, g, s)
      _  <- refresh   // g is no longer top-level, so its stale tree is removed
      b  <- groupStanding(s)
      _  <- refresh   // gb's tree stores it again
      a  <- groupStanding(s)
      t  <- calculatedTrees(g, gb)
    yield
      assertEquals(b, viable("PENDING"))
      assertEquals(a, viable())
      assertEquals(t, Set(gb))

  test("moving a group into its own subgroup fails, rather than marking forever"):
    for
      p  <- createProgram
      o  <- newObservation(p)
      g  <- group(p, Nil)
      s  <- group(p, List(o), parent = Some(g))
      _  <- refresh
      r  <- moveGroupAs(pi, g, Some(s)).attempt.timeout(30.seconds)
      js <- groupStanding(s)
    yield
      assert(r.isLeft, "a cycle should be rejected")
      assertEquals(js, viable())

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

  test("a deleted observation in a deleted group stands as one outside any group"):
    for
      p  <- createProgram
      o  <- newObservation(p)
      g  <- group(p, List(o), min = Some(1))
      _  <- refresh
      _  <- deleteObservation(pi, o)
      _  <- deleteGroup(g)
      _  <- refresh
      js <- observationStanding(o)
    yield assertEquals(js, viable())

  val SetInactive: Command[Observation.Id] =
    sql"UPDATE t_observation SET c_workflow_user_state = 'inactive' WHERE c_observation_id = $observation_id".command

  val SetQaOverrideFail: Command[Observation.Id] =
    sql"UPDATE t_observation SET c_qa_state_override = 'fail' WHERE c_observation_id = $observation_id".command

  test("a QA override marks the tree, and a rejected completion no longer completes it"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), min = Some(1))
      _  <- completeObservation(o1)
      _  <- refresh
      c  <- observationStanding(o2)
      _  <- withSession(_.execute(SetQaOverrideFail)(o1))
      b  <- observationStanding(o2)
      _  <- refresh
      a  <- observationStanding(o2)
    yield
      assertEquals(c, standingJson("READY", "COMPLETE", Some(g)))
      assertEquals(b, standingJson("PENDING", "COMPLETE", Some(g)))
      assertEquals(a, viable())

  test("a suspension marks the tree"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2))
      _  <- startObservation(o1)
      _  <- runObscalcUpdate(p, o1)
      _  <- refresh
      _  <- setObservationWorkflowState(staff, o1, ObservationWorkflowState.Suspended)
      js <- groupStanding(g)
    yield assertEquals(js, viable("PENDING"))

  test("a dataset QA edit leaves the tree ready while its observation executes"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      _  <- group(p, List(o1, o2), min = Some(1))
      v  <- recordVisitAs(serviceUser, o1)
      ss <- firstScienceAtomStepIds(serviceUser, o1)
      _  <- addStepEventAs(serviceUser, ss.head, v, StepStage.StartStep)
      d  <- recordDatasetAs(serviceUser, ss.head, v, "N18630101S0001.fits")
      _  <- runObscalcUpdate(p, o1)
      _  <- refresh
      _  <- updateDatasets(staff, DatasetQaState.Fail, List(d))
      js <- observationStanding(o2)
    yield assertEquals(js, viable())

  test("a dataset QA edit marks the tree once its observation is complete"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), min = Some(1))
      v  <- recordVisitAs(serviceUser, o1)
      ss <- scienceStepIds(serviceUser, o1)
      _  <- addStepEventAs(serviceUser, ss.head, v, StepStage.StartStep)
      d  <- recordDatasetAs(serviceUser, ss.head, v, "N18630101S0002.fits")
      _  <- ss.traverse_(sid => addEndStepEvent(sid, v))
      _  <- runObscalcUpdate(p, o1)
      _  <- refresh
      b  <- observationStanding(o2)
      _  <- updateDatasets(staff, DatasetQaState.Fail, List(d))
      a  <- observationStanding(o2)
    yield
      assertEquals(b, standingJson("READY", "COMPLETE", Some(g)))
      assertEquals(a, standingJson("PENDING", "COMPLETE", Some(g)))

  test("writing observation or group columns without changing them leaves the tree ready"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      g  <- group(p, List(o1))
      _  <- refresh
      _  <- withSession(_.execute(sql"UPDATE t_observation SET c_instrument = c_instrument, c_workflow_user_state = c_workflow_user_state WHERE c_observation_id = $observation_id".command)(o1))
      _  <- withSession(_.execute(sql"UPDATE t_group SET c_ordered = c_ordered, c_min_required = c_min_required WHERE c_group_id = $group_id".command)(g))
      js <- groupStanding(g)
    yield assertEquals(js, viable())

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

  test("time accounting moving a visitor observation's charge end marks its tree"):
    val maxI = 1.hrTimeSpan
    for
      p  <- createProgram
      t  <- createTargetWithProfileAs(pi, p)
      o1 <- createVisitorModeObservationAs(pi, p, VisitorObservingModeType.ZorroSpeckle, t)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), maxI = Some(maxI))
      _  <- addSlewEventAs(serviceUser, o1, SlewStage.StartSlew)
      _  <- declareCompleteDirectly(o1)
      _  <- refresh
      c  <- completionTime(o1)   // the slew
      rb <- reevaluateAt(g)
      // As the sweep does once the next visit at the site starts.
      _  <- withSession(_.execute(sql"""
              UPDATE t_visit
              SET    c_ta_visitor_end = c_end + interval '2 hours'
              WHERE  c_observation_id = $observation_id
            """.command)(o1))
      js <- observationStanding(o2)
      _  <- refresh
      ra <- reevaluateAt(g)
    yield
      assertEquals(rb, Some(c +| maxI))
      assertEquals(js, viable("PENDING"))
      assertEquals(ra, Some(c +| 2.hrTimeSpan +| maxI))

  test("an ongoing group's window inside a group already decided sets no re-evaluation time"):
    for
      p  <- createProgram
      a  <- newObservation(p)
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(a), min = Some(1))
      s  <- group(p, List(o1, o2), maxI = Some(1.hrTimeSpan), parent = Some(g))
      _  <- completeObservation(o1)
      _  <- completeObservation(a)
      _  <- refresh
      st <- groupStatus(s)
      js <- groupStanding(s)
      ra <- reevaluateAt(g)
    yield
      assertEquals(st, "ONGOING")
      assertEquals(js, standingJson("READY", "COMPLETE", Some(g)))
      assertEquals(ra, None)

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

  // Failure and retry

  // Runs `body` as `root`'s tree is written to t_group_standing_calc, after
  // its marks are claimed: as its calculation is stored, and also as its
  // retry is scheduled (which sets c_retry_at) if `retryToo`.
  def calcTrigger(root: Group.Id, body: String, retryToo: Boolean): Resource[IO, Unit] =
    val when = if retryToo then "" else " AND NEW.c_retry_at IS NULL"
    Resource.make(
      withSession: s =>
        s.execute(sql"""
          CREATE OR REPLACE FUNCTION test_fail_group_standing() RETURNS trigger AS $$$$
          BEGIN
            #$body
          END;
          $$$$ LANGUAGE plpgsql
        """.command) *>
        s.execute(sql"""
          CREATE TRIGGER test_fail_group_standing
            BEFORE INSERT OR UPDATE ON t_group_standing_calc
            FOR EACH ROW
            WHEN (NEW.c_group_id = '#${root.toString}'#$when)
            EXECUTE FUNCTION test_fail_group_standing()
        """.command).void
    )(_ =>
      withSession(_.execute(sql"DROP TRIGGER IF EXISTS test_fail_group_standing ON t_group_standing_calc".command).void)
    )

  // Makes recalculating `root`'s tree fail, while still letting a retry be
  // scheduled.
  def failingCalculation(root: Group.Id): Resource[IO, Unit] =
    calcTrigger(root, "RAISE EXCEPTION 'test failure';", retryToo = false)

  // The failure count, and whether a retry is scheduled in the future.
  def retryState(root: Group.Id): IO[(Int, Boolean)] =
    withSession: s =>
      s.unique(sql"""
        SELECT c_failure_count, COALESCE(c_retry_at > now(), false)
        FROM   t_group_standing_calc
        WHERE  c_group_id = $group_id
      """.query(int4 *: bool))(root)

  def markCount(root: Group.Id): IO[Long] =
    withSession(_.unique(sql"SELECT COUNT(*) FROM t_group_standing_mark WHERE c_root_group_id = $group_id".query(int8))(root))

  // As if the clock had reached the tree's retry time.
  def reachRetryAt(root: Group.Id): IO[Unit] =
    withSession(_.execute(sql"UPDATE t_group_standing_calc SET c_retry_at = now() - interval '1 second' WHERE c_group_id = $group_id".command)(root).void)

  test("a tree that fails to calculate is retried later, without holding up the others"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g1 <- group(p, List(o1))
      g2 <- group(p, List(o2))
      _  <- refresh
      r  <- failingCalculation(g1).use: _ =>
              for
                _  <- setMinimumRequired(g1, 1)   // marked first, so tried first
                _  <- setMinimumRequired(g2, 1)
                _  <- refresh
                j1 <- groupStanding(g1)
                j2 <- groupStanding(g2)
                rs <- retryState(g1)
                mc <- markCount(g1)
              yield (j1, j2, rs, mc)
      (j1, j2, rs, mc) = r
    yield
      assertEquals(j1, viable("RETRY"))
      assertEquals(j2, viable())
      assertEquals(rs, (1, true))
      assertEquals(mc, 0L)   // the claimed marks don't come back with the rollback

  test("a tree waiting to retry isn't recalculated until its retry time, even when edited"):
    for
      p  <- createProgram
      o  <- newObservation(p)
      g  <- group(p, List(o))
      _  <- refresh
      _  <- failingCalculation(g).use(_ => setMinimumRequired(g, 1) *> refresh)
      _  <- withSession(_.execute(SetInactive)(o))
      _  <- refresh
      b  <- groupStanding(g)
      rb <- retryState(g)
      mb <- markCount(g)
      _  <- reachRetryAt(g)
      _  <- refresh
      a  <- groupStanding(g)
      ra <- retryState(g)
      ma <- markCount(g)
    yield
      assertEquals(b, viable("RETRY"))   // marked, but waiting for the retry time
      assertEquals(rb, (1, true))
      assert(mb > 0, "the edit's marks wait for the retry")
      assertEquals(a, viable())
      assertEquals(ra, (0, false))
      assertEquals(ma, 0L)

  test("a tree whose first calculation fails reads retry, not never calculated"):
    for
      p  <- createProgram
      o  <- newObservation(p)
      s  <- newObservation(p)
      g  <- group(p, List(o))
      sg <- group(p, List(s), parent = Some(g))
      _  <- failingCalculation(g).use(_ => refresh)
      jg <- groupStanding(g)
      js <- groupStanding(sg)
      jo <- observationStanding(o)
      _  <- reachRetryAt(g)
      _  <- refresh
      ja <- groupStanding(sg)
    yield
      assertEquals(jg, viable("RETRY"))
      assertEquals(js, viable("RETRY"))
      assertEquals(jo, viable("RETRY"))
      assertEquals(ja, viable())

  test("each failure in a row doubles the time to the retry"):
    for
      p  <- createProgram
      o  <- newObservation(p)
      g  <- group(p, List(o))
      _  <- refresh
      d  <- failingCalculation(g).use: _ =>
              for
                _ <- setMinimumRequired(g, 1)
                _ <- refresh
                _ <- reachRetryAt(g)
                _ <- refresh
                d <- withSession(_.unique(sql"""
                       SELECT EXTRACT(EPOCH FROM c_retry_at - now()) :: int4
                       FROM   t_group_standing_calc
                       WHERE  c_group_id = $group_id
                     """.query(int4))(g))
              yield d
      rs <- retryState(g)
    yield
      assertEquals(rs, (2, true))
      assert(d > 60 && d <= 120, s"expected a 2 minute backoff, but the retry is $d seconds away")

  // Keeps the messages logged, most recent first.
  final class CapturingLogger(messages: Ref[IO, List[String]]) extends Logger[IO]:
    private def add(message: => String): IO[Unit] = messages.update(message :: _)
    def error(message: => String): IO[Unit]               = add(message)
    def warn(message: => String): IO[Unit]                = add(message)
    def info(message: => String): IO[Unit]                = add(message)
    def debug(message: => String): IO[Unit]               = add(message)
    def trace(message: => String): IO[Unit]               = add(message)
    def error(t: Throwable)(message: => String): IO[Unit] = add(message)
    def warn(t: Throwable)(message: => String): IO[Unit]  = add(message)
    def info(t: Throwable)(message: => String): IO[Unit]  = add(message)
    def debug(t: Throwable)(message: => String): IO[Unit] = add(message)
    def trace(t: Throwable)(message: => String): IO[Unit] = add(message)

  // Refreshes, returning what the refresh logged, in order.  A failure of the
  // refresh itself (its session failing, say) is ignored.
  def refreshLogging: IO[List[String]] =
    Ref.of[IO, List[String]](Nil).flatMap: messages =>
      refreshGroupStandingsAs(serviceUser, Some(CapturingLogger(messages))).attempt *>
      messages.get.map(_.reverse)

  // Two trees, g1 marked before g2 so that it is tried first, refreshed while
  // `failure` is in place for g1.  The standings of both, g1's mark count and
  // what the refresh logged.
  def refreshWithFailure(failure: Group.Id => Resource[IO, Unit]): IO[(Json, Json, Long, List[String])] =
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g1 <- group(p, List(o1))
      g2 <- group(p, List(o2))
      _  <- refresh
      ms <- failure(g1).use: _ =>
              setMinimumRequired(g1, 1) *>
              setMinimumRequired(g2, 1) *>
              refreshLogging
      j1 <- groupStanding(g1)
      j2 <- groupStanding(g2)
      mc <- markCount(g1)
    yield (j1, j2, mc, ms)

  def count(messages: List[String], prefix: String): Int =
    messages.count(_.startsWith(prefix))

  test("a tree whose retry can't be scheduled either doesn't hold up the others while the session answers"):
    refreshWithFailure(calcTrigger(_, "RAISE EXCEPTION 'test failure';", retryToo = true)).map: (j1, j2, mc, ms) =>
      assertEquals(j1, viable("PENDING"))   // nothing scheduled, so its marks are back
      assert(mc > 0, "the claimed marks come back with the rollback")
      assertEquals(j2, viable())
      assertEquals(count(ms, "Failed to schedule a retry"), 1)
      assertEquals(count(ms, "Stopping"), 0)

  test("a refresh whose database session fails leaves the rest for the next one"):
    // Ending its own connection fails the calculation, the retry, and the check
    // that the session still answers.  With the session gone every later tree
    // would fail too, so only the first is tried.
    refreshWithFailure(calcTrigger(_, "PERFORM pg_terminate_backend(pg_backend_pid());", retryToo = true)).map: (j1, j2, _, ms) =>
      assertEquals(j1, viable("PENDING"))
      assertEquals(j2, viable("PENDING"))
      assertEquals(count(ms, "Failed to calculate"), 1)
      assertEquals(count(ms, "Failed to schedule a retry"), 1)
      assertEquals(ms.filter(_.startsWith("Stopping")), List("Stopping the group standing refresh, whose database session failed, with 1 trees left for the next one"))

  test("a tree another refresh has just recalculated isn't recalculated again from a stale list"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g1 <- group(p, List(o1))
      g2 <- group(p, List(o2))
      _  <- refresh
      // Both listed as due, g1 first.  Storing g1's calculation does what
      // another refresher finishing g2 would: claims its marks and stores its
      // calculation (with a re-evaluation time a day off, to recognize it).
      // So when this refresh reaches g2, it has no marks and isn't due.
      body = s"""
        DELETE FROM t_group_standing_mark WHERE c_root_group_id = '$g2';
        UPDATE t_group_standing_calc SET c_reevaluate_at = now() + interval '1 day' WHERE c_group_id = '$g2';
        RETURN NEW;
      """
      n  <- calcTrigger(g1, body, retryToo = false).use: _ =>
              setMinimumRequired(g1, 1) *>
              setMinimumRequired(g2, 1) *>
              refreshGroupStandingsAs(serviceUser)
      ra <- reevaluateAt(g2)
      now <- withSession(_.unique(sql"SELECT now()::timestamp".query(core_timestamp)))
    yield
      assertEquals(n, 1)
      assert(ra.exists(_ > now +| 23.hourTimeSpan), s"g2 was recalculated again: re-evaluation time $ra")

  test("a tree that deadlocks with another refresh is left for the next refresh, with no retry wait"):
    refreshWithFailure(calcTrigger(_, "RAISE EXCEPTION 'test deadlock' USING ERRCODE = 'deadlock_detected';", retryToo = false)).map: (j1, j2, mc, ms) =>
      assertEquals(j1, viable("PENDING"))   // not RETRY: its marks are back
      assert(mc > 0, "the claimed marks come back with the rollback")
      assertEquals(j2, viable())
      assertEquals(count(ms, "Failed to calculate"), 0)
      assertEquals(count(ms, "Recalculating the group standing"), 1)

