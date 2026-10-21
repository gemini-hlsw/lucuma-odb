// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.ACursorOps
import lucuma.odb.util.Codecs.observation_id
import skunk.syntax.all.*

// The rules are tested in lucuma-core (GroupProgressEvaluatorSuite).  These
// tests check that the facts reach the evaluator and the result reaches the API.
class groupProgress extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(2)
    )

  def progressQuery(gid: Group.Id): String =
    s"""
      query {
        group(groupId: "$gid") {
          progress {
            status
            completed
            required
            nextWindow { start end }
            violations
          }
        }
      }
    """

  def progress(gid: Group.Id): IO[Json] =
    query(pi, progressQuery(gid)).map(_.hcursor.downFields("group", "progress").focus.getOrElse(Json.Null))

  def newObservation(pid: Program.Id): IO[Observation.Id] =
    createTargetWithProfileAs(pi, pid).flatMap(t => createGmosNorthLongSlitObservationAs(pi, pid, List(t)))

  def completeObservation(oid: Observation.Id): IO[Unit] =
    for
      ss <- scienceStepIds(serviceUser, oid)
      v  <- recordVisitAs(serviceUser, oid)
      _  <- ss.traverse(sid => addEndStepEvent(sid, v))
    yield ()

  def completionTime(oid: Observation.Id): IO[Timestamp] =
    query(pi, s"""query { observation(observationId: "$oid") { execution { timestamps { completionTime } } } }""")
      .map(_.hcursor.downFields("observation", "execution", "timestamps", "completionTime").require[Timestamp])

  def group(
    pid:      Program.Id,
    oids:     List[Observation.Id],
    min:      Option[Int]      = None,
    minI:     Option[TimeSpan] = None,
    maxI:     Option[TimeSpan] = None,
    parent:   Option[Group.Id] = None
  ): IO[Group.Id] =
    createGroupAs(
      pi,
      pid,
      parentGroupId   = parent,
      minRequired     = min.map(m => NonNegShort.unsafeFrom(m.toShort)),
      minimumInterval = minI,
      maximumInterval = maxI,
      initialContents = Some(oids.map(_.asRight))
    )

  def startObservation(oid: Observation.Id): IO[Unit] =
    for
      v  <- recordVisitAs(serviceUser, oid)
      ss <- firstScienceAtomStepIds(serviceUser, oid)
      _  <- ss.take(1).traverse(sid => addEndStepEvent(sid, v))
    yield ()

  def setSameNight(gid: Group.Id): IO[Unit] =
    query(pi, s"""mutation { updateGroups(input: { SET: { sameNight: true }, WHERE: { id: { EQ: "$gid" } } }) { groups { id } } }""").void

  def setVisitSiteDirectly(oid: Observation.Id, site: String): IO[Unit] =
    withFreshSession: s =>
      s.execute(
        sql"update t_visit set c_site = #$site where c_observation_id = $observation_id".command
      )(oid).void

  test("pending: nothing started"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2))
      js <- progress(g)
    yield assertEquals(
      js,
      json"""
        {
          "status": "PENDING",
          "completed": 0,
          "required": 2,
          "nextWindow": null,
          "violations": []
        }
      """
    )

  test("ongoing: a window with only a minimum interval has no end"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), minI = Some(1.hrTimeSpan))
      _  <- completeObservation(o1)
      t  <- completionTime(o1)
      js <- progress(g)
    yield assertEquals(
      js.hcursor.downField("nextWindow").focus,
      Some(json"""{ "start": ${t +| 1.hrTimeSpan}, "end": null }""")
    )

  test("ongoing: the window runs from the completion to the maximum interval"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), maxI = Some(1.hrTimeSpan))
      _  <- completeObservation(o1)
      t  <- completionTime(o1)
      js <- progress(g)
    yield assertEquals(
      js,
      json"""
        {
          "status": "ONGOING",
          "completed": 1,
          "required": 2,
          "nextWindow": { "start": $t, "end": ${t +| 1.hrTimeSpan} },
          "violations": []
        }
      """
    )

  test("complete: the minimum is met"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), min = Some(1))
      _  <- completeObservation(o1)
      js <- progress(g)
    yield
      assertEquals(js.hcursor.downField("status").require[String], "COMPLETE")

  test("unschedulable: an inactive observation leaves too few to reach the minimum"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), min = Some(2))
      _  <- setObservationWorkflowState(pi, o2, ObservationWorkflowState.Inactive)
      js <- progress(g)
    yield assertEquals(
      js,
      json"""
        {
          "status": "UNSCHEDULABLE",
          "completed": 0,
          "required": 2,
          "nextWindow": null,
          "violations": ["INSUFFICIENT_ELEMENTS"]
        }
      """
    )

  test("nested groups are evaluated together, in one query"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      o3 <- newObservation(p)
      g  <- group(p, List(o1))
      s  <- group(p, List(o2, o3), min = Some(1), parent = Some(g))
      _  <- completeObservation(o2)
      js <- query(pi, s"""query { program(programId: "$p") { allGroupElements { group { id progress { status completed required } } } } }""")
    yield
      val byGroup: Map[Group.Id, Json] =
        js.hcursor.downFields("program", "allGroupElements").require[List[Json]]
          .flatMap(_.hcursor.downField("group").focus.filterNot(_.isNull))
          .map(j => j.hcursor.downField("id").require[Group.Id] -> j.hcursor.downField("progress").focus.get)
          .toMap
      // The subgroup is complete, and counts as one of its parent's two elements.
      assertEquals(byGroup(s), json"""{ "status": "COMPLETE", "completed": 1, "required": 1 }""")
      assertEquals(byGroup(g), json"""{ "status": "ONGOING", "completed": 1, "required": 2 }""")

  test("querying groups one at a time, each evaluated from its own subtree"):
    for
      p  <- createProgram
      o0 <- newObservation(p)               // ungrouped, at the top level
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      o3 <- newObservation(p)
      g  <- group(p, List(o1))
      s  <- group(p, List(o2), parent = Some(g))
      x  <- group(p, List(o3))              // an unrelated group
      _  <- completeObservation(o0)
      _  <- completeObservation(o1)
      _  <- completeObservation(o3)
      gp <- progress(g)
      sp <- progress(s)
      xp <- progress(x)
    yield
      def summary(j: Json): (String, Int, Int) =
        (j.hcursor.downField("status").require[String], j.hcursor.downField("completed").require[Int], j.hcursor.downField("required").require[Int])
      assertEquals(summary(gp), ("ONGOING", 1, 2))
      assertEquals(summary(sp), ("PENDING", 0, 1))
      assertEquals(summary(xp), ("COMPLETE", 1, 1))

  def inactivateDirectly(oid: Observation.Id): IO[Unit] =
    withFreshSession: s =>
      s.execute(
        sql"update t_observation set c_workflow_user_state = 'inactive' where c_observation_id = $observation_id".command
      )(oid).void

  // A science observation and a per-observation calibration, in one group.
  def calibrationGroup(role: CalibrationRole): IO[(Group.Id, Observation.Id, Observation.Id)] =
    for
      p   <- createProgram
      sci <- newObservation(p)
      cal <- newObservation(p)
      g   <- group(p, List(sci, cal))
      _   <- setObservationCalibrationRole(List(cal), role)
    yield (g, sci, cal)

  def statusAndRequired(js: Json): (String, Int) =
    (js.hcursor.downField("status").require[String], js.hcursor.downField("required").require[Int])

  test("a telluric follows its science observation's inactive state"):
    for
      (g, sci, _) <- calibrationGroup(CalibrationRole.Telluric)
      _           <- inactivateDirectly(sci)
      js          <- progress(g)
    yield assertEquals(statusAndRequired(js), ("EMPTY", 0))

  test("a declined telluric is left out though its science observation is active"):
    for
      (g, _, cal) <- calibrationGroup(CalibrationRole.Telluric)
      _             <- inactivateDirectly(cal)
      js            <- progress(g)
    yield assertEquals(statusAndRequired(js), ("PENDING", 1))

  test("a daytime pinhole set inactive itself still follows its active science observation"):
    for
      (g, _, cal) <- calibrationGroup(CalibrationRole.DaytimePinhole)
      _             <- inactivateDirectly(cal)
      js            <- progress(g)
    yield assertEquals(statusAndRequired(js), ("PENDING", 2))

  test("an observation that starts within the minimum interval sinks the group at once"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2), minI = Some(1.hrTimeSpan))
      _  <- completeObservation(o1)
      _  <- startObservation(o2)   // just after o1 completes, so within the hour
      js <- progress(g)
    yield
      assertEquals(js.hcursor.downField("status").require[String], "UNSCHEDULABLE")
      assertEquals(
        js.hcursor.downField("violations").focus.get,
        json"""["MINIMUM_INTERVAL"]"""
      )

  test("a completed observation's site is where its visit was, not its instrument's"):
    for
      p  <- createProgram
      o1 <- newObservation(p)      // GMOS North
      o2 <- newObservation(p)      // GMOS North
      g  <- group(p, List(o1, o2))
      _  <- setSameNight(g)
      _  <- completeObservation(o1)
      _  <- setVisitSiteDirectly(o1, "'gs'")
      js <- progress(g)
    yield assertEquals(
      js.hcursor.downFields("violations").focus.get,
      json"""["SAME_NIGHT_MULTIPLE_SITES"]"""
    )

  test("empty: a group with only inactive observations"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      g  <- group(p, List(o1), min = Some(1))
      _  <- setObservationWorkflowState(pi, o1, ObservationWorkflowState.Inactive)
      js <- progress(g)
    yield assertEquals(
      js,
      json"""
        {
          "status": "EMPTY",
          "completed": 0,
          "required": 0,
          "nextWindow": null,
          "violations": []
        }
      """
    )

  test("a deleted group has no progress"):
    for
      p  <- createProgram
      g  <- group(p, Nil)
      _  <- deleteGroup(pi, g)
      js <- progress(g)
    yield assertEquals(js, Json.Null)
