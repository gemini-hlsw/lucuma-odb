// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package scheduler

import cats.effect.IO
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.util.Codecs.*
import org.http4s.*
import org.http4s.implicits.*
import skunk.codec.all.text
import skunk.implicits.*

import java.time.Instant

class visibilityChanges extends SchedulerRoutesSuite with ExecutionTestSupportForGmos:

  private val Epoch:  Instant = Instant.EPOCH
  private val Future: Instant = Instant.parse("2999-01-01T00:00:00Z")

  private def visibilityRequest(user: User, since: Instant): IO[Request[IO]] =
    headers(user).map: hs =>
      Request[IO](
        method  = Method.GET,
        uri     = uri"scheduler/visibility-changes".withQueryParam("since", since.toString),
        headers = hs
      )

  private def fetchVisibilityChanges(user: User, since: Instant): IO[(Status, String)] =
    visibilityRequest(user, since).flatMap: request =>
      withRoutes(user, request).flatMap: response =>
        response.as[String].map(response.status -> _)

  private def fetchGzip(user: User, since: Instant): IO[(Status, String)] =
    headers(user, gzip = true).flatMap: hs =>
      val request = Request[IO](
        method  = Method.GET,
        uri     = uri"scheduler/visibility-changes".withQueryParam("since", since.toString),
        headers = hs
      )
      withRoutes(user, request).flatMap: response =>
        decompress(response).map(response.status -> _)

  private def obsInvalidation(oid: Observation.Id): IO[Instant] =
    withSession: s =>
      s.unique(
        sql"SELECT c_last_visibility_invalidation FROM t_observation_visibility WHERE c_observation_id = $observation_id"
          .query(core_timestamp)
      )(oid).map(_.toInstant)

  private def observationInvalidation(oid: Observation.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            WHERE: { id: { EQ: "$oid" } }
            SET: { constraintSet: { cloudExtinction: THREE_POINT_ZERO } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def targetInvalidation(tid: Target.Id): IO[Instant] =
    withSession: s =>
      s.unique(
        sql"SELECT c_last_visibility_invalidation FROM t_target_visibility WHERE c_target_id = $target_id"
          .query(core_timestamp)
      )(tid).map(_.toInstant)

  private def bumpRadialVelocity(tid: Target.Id): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"UPDATE t_target SET c_sid_rv = 999.0 WHERE c_target_id = $target_id".command
      )(tid).void

  private def bumpParallax(tid: Target.Id): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"UPDATE t_target SET c_sid_parallax = 1000000 WHERE c_target_id = $target_id".command
      )(tid).void

  // Raw SQL so only the relevant child/parent row changes, exercising the trigger directly.
  private def addTimingWindow(oid: Observation.Id): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"INSERT INTO t_timing_window (c_observation_id, c_inclusion, c_start) VALUES ($observation_id, 'include', '2030-01-01T00:00:00')".command
      )(oid).void

  private def changeProgramActivePeriod(pid: Program.Id): IO[Unit] =
    withSession: s =>
      s.transaction.use: _ =>
        s.unique(sql"select set_config('lucuma.user', $text, true)".query(text))(Gid[User.Id].show(pi.id)) >>
          s.execute(
            sql"UPDATE t_program SET c_active_end = '2098-06-30' WHERE c_program_id = $program_id".command
          )(pid).void

  private def hasObs(body: String, oid: Observation.Id): Boolean =
    body.linesIterator.contains(Gid[Observation.Id].show(oid))

  private def hasTarget(body: String, tid: Target.Id): Boolean =
    body.linesIterator.contains(Gid[Target.Id].show(tid))

  test("non-service user is forbidden"):
    fetchVisibilityChanges(pi, Epoch).map: (status, _) =>
      assertEquals(status, Status.Forbidden)

  test("an untracked observation and its target are not listed"):
    for
      p       <- createProgram
      t       <- createTargetWithProfileAs(pi, p)
      o       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      (st, b) <- fetchVisibilityChanges(serviceUser, Epoch)
    yield
      assertEquals(st, Status.Ok)
      assert(!hasObs(b, o))
      assert(!hasTarget(b, t))

  test("a ready observation"):
    for
      p         <- createProgram
      t         <- createTargetWithProfileAs(pi, p)
      o         <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _         <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      (st1, b1) <- fetchVisibilityChanges(serviceUser, Epoch)
      _         <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ongoing)
      (st2, b2) <- fetchVisibilityChanges(serviceUser, Epoch)
    yield
      // Ready obs
      assertEquals(st1, Status.Ok)
      assert(hasObs(b1, o))
      assert(hasTarget(b1, t))
      // Ongoing obs
      assertEquals(st2, Status.Ok)
      assert(hasObs(b2, o))
      assert(hasTarget(b2, t))

  test("an observation with visibility-relevant changes appears in the response"):
    for
      p        <- createProgram
      t        <- createTargetWithProfileAs(pi, p)
      o        <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _        <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      before   <- obsInvalidation(o)
      cursor    = before.plusMillis(1)
      (_,  b0) <- fetchVisibilityChanges(serviceUser, cursor)
      // A visibility-relevant constraint edit re-stamps the observation after the cursor.
      _        <- observationInvalidation(o)
      after    <- obsInvalidation(o)
      (st, b)  <- fetchVisibilityChanges(serviceUser, cursor)
    yield
      // not present after the last change
      assert(!hasObs(b0, o))
      assert(after.isAfter(cursor))
      assertEquals(st, Status.Ok)
      // Trigger updated the observation
      assert(hasObs(b, o))

  test("a target updated after a given 'since' appears in the response"):
    for
      p        <- createProgram
      t        <- createTargetWithProfileAs(pi, p)
      o        <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _        <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      before   <- targetInvalidation(t)
      cursor    = before.plusMillis(1)
      (_,  b0) <- fetchVisibilityChanges(serviceUser, cursor)
      // A visibility-relevant edit re-stamps the target after the cursor.
      _        <- updateTargetPropertiesAs(pi, t, Coordinates(RightAscension.fromDoubleDegrees(42.0), Declination.fromDoubleDegrees(17.0).get))
      after    <- targetInvalidation(t)
      (st, b)  <- fetchVisibilityChanges(serviceUser, cursor)
    yield
      assert(!hasTarget(b0, t))
      assert(after.isAfter(cursor))
      assertEquals(st, Status.Ok)
      assert(hasTarget(b, t))

  test("a target whose radial velocity changes appears in the response"):
    for
      p        <- createProgram
      t        <- createTargetWithProfileAs(pi, p)
      o        <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _        <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      before   <- targetInvalidation(t)
      cursor    = before.plusMillis(1)
      (_,  b0) <- fetchVisibilityChanges(serviceUser, cursor)
      // An RV-only edit re-stamps the target after the cursor.
      _        <- bumpRadialVelocity(t)
      after    <- targetInvalidation(t)
      (st, b)  <- fetchVisibilityChanges(serviceUser, cursor)
    yield
      assert(!hasTarget(b0, t))
      assert(after.isAfter(cursor))
      assertEquals(st, Status.Ok)
      assert(hasTarget(b, t))

  test("a target whose parallax changes appears in the response"):
    for
      p        <- createProgram
      t        <- createTargetWithProfileAs(pi, p)
      o        <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _        <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      before   <- targetInvalidation(t)
      cursor    = before.plusMillis(1)
      (_,  b0) <- fetchVisibilityChanges(serviceUser, cursor)
      // A parallax-only edit re-stamps the target after the cursor.
      _        <- bumpParallax(t)
      after    <- targetInvalidation(t)
      (st, b)  <- fetchVisibilityChanges(serviceUser, cursor)
    yield
      assert(!hasTarget(b0, t))
      assert(after.isAfter(cursor))
      assertEquals(st, Status.Ok)
      assert(hasTarget(b, t))

  test("a timing-window change surfaces its observation"):
    for
      p         <- createProgram
      t         <- createTargetWithProfileAs(pi, p)
      o         <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _         <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      before    <- obsInvalidation(o)
      cursor     = before.plusMillis(1)
      (_,  b0)  <- fetchVisibilityChanges(serviceUser, cursor)
      // A timing-window insert (a child-table write) re-stamps the parent observation.
      _         <- addTimingWindow(o)
      after     <- obsInvalidation(o)
      (st, b)   <- fetchVisibilityChanges(serviceUser, cursor)
    yield
      assert(!hasObs(b0, o))
      assert(after.isAfter(cursor))
      assertEquals(st, Status.Ok)
      assert(hasObs(b, o))

  test("a program active-period change surfaces every observation in the program"):
    for
      p         <- createProgram
      t         <- createTargetWithProfileAs(pi, p)
      o1        <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      o2        <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _         <- setCalculatedWorkflowState(o1, ObservationWorkflowState.Ready)
      _         <- setCalculatedWorkflowState(o2, ObservationWorkflowState.Ready)
      before1   <- obsInvalidation(o1)
      before2   <- obsInvalidation(o2)
      cursor     = (if before1.isAfter(before2) then before1 else before2).plusMillis(1)
      (_,  b0)  <- fetchVisibilityChanges(serviceUser, cursor)
      // Editing the program active period fans out and re-stamps every program observation.
      _         <- changeProgramActivePeriod(p)
      after1    <- obsInvalidation(o1)
      after2    <- obsInvalidation(o2)
      (st, b)   <- fetchVisibilityChanges(serviceUser, cursor)
    yield
      assert(!hasObs(b0, o1))
      assert(!hasObs(b0, o2))
      assert(after1.isAfter(cursor))
      assert(after2.isAfter(cursor))
      assertEquals(st, Status.Ok)
      assert(hasObs(b, o1))
      assert(hasObs(b, o2))

  test("an observation that was completed after a given 'since' is not listed"):
    for
      p       <- createProgram
      t       <- createTargetWithProfileAs(pi, p)
      o       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _       <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      _       <- setCalculatedWorkflowState(o, ObservationWorkflowState.Completed)
      (st, b) <- fetchVisibilityChanges(serviceUser, Epoch)
    yield
      assertEquals(st, Status.Ok)
      assert(!hasObs(b, o), s"completed obs should not be listed: $b")

  test("a 'since' equal to the invalidation time is inclusive"):
    for
      p       <- createProgram
      t       <- createTargetWithProfileAs(pi, p)
      o       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _       <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      ts      <- obsInvalidation(o)
      (st, b) <- fetchVisibilityChanges(serviceUser, ts)
    yield
      assertEquals(st, Status.Ok)
      assert(hasObs(b, o), s"obs stamped exactly at 'since' should be listed: $b")

  test("a gzip-encoded response can be decompressed"):
    for
      p       <- createProgram
      t       <- createTargetWithProfileAs(pi, p)
      o       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _       <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      (st, b) <- fetchGzip(serviceUser, Epoch)
    yield
      assertEquals(st, Status.Ok)
      assert(hasObs(b, o),    s"ready obs should be listed (gzip): $b")
      assert(hasTarget(b, t), s"target of ready obs should be listed (gzip): $b")

  test("a 'since' in the future excludes everything"):
    for
      p       <- createProgram
      t       <- createTargetWithProfileAs(pi, p)
      o       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _       <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      (st, b) <- fetchVisibilityChanges(serviceUser, Future)
    yield
      assertEquals(st, Status.Ok)
      assert(!hasObs(b, o),    s"obs should not be listed for a future 'since': $b")
      assert(!hasTarget(b, t), s"target should not be listed for a future 'since': $b")

  test("a deleted observation is not listed even though it remains ready/ongoing"):
    for
      p       <- createProgram
      t       <- createTargetWithProfileAs(pi, p)
      o       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _       <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      _       <- deleteObservation(pi, o)
      (st, b) <- fetchVisibilityChanges(serviceUser, Epoch)
    yield
      assertEquals(st, Status.Ok)
      assert(!hasObs(b, o), s"deleted obs should not be listed: $b")

  test("a deleted target is not listed even though its observation is tracked"):
    for
      p       <- createProgram
      t       <- createTargetWithProfileAs(pi, p)
      o       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _       <- setCalculatedWorkflowState(o, ObservationWorkflowState.Ready)
      _       <- deleteTargetAs(pi, t)
      (st, b) <- fetchVisibilityChanges(serviceUser, Epoch)
    yield
      assertEquals(st, Status.Ok)
      assert(!hasTarget(b, t), s"deleted target should not be listed: $b")
