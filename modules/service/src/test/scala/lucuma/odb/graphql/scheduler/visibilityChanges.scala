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
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.util.Codecs.*
import org.http4s.*
import org.http4s.implicits.*
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
        sql"SELECT c_last_visibility_invalidation FROM t_observation WHERE c_observation_id = $observation_id"
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
        sql"SELECT c_last_visibility_invalidation FROM t_target WHERE c_target_id = $target_id"
          .query(core_timestamp)
      )(tid).map(_.toInstant)

  private def hasObs(body: String, oid: Observation.Id): Boolean =
    body.linesIterator.contains(s"OBSERVATION\t${Gid[Observation.Id].show(oid)}")

  private def hasTarget(body: String, tid: Target.Id): Boolean =
    body.linesIterator.contains(s"TARGET\t${Gid[Target.Id].show(tid)}")

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
