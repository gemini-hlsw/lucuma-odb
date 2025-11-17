// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.issue.shortcut

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.service.TrackingService

import java.time.Instant

// Test ephemeris caching
class ShortCut_6900 extends OdbSuite:

  val pi = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi)

  object Intervals:
    val start: Timestamp = Timestamp.fromInstantTruncatedAndBounded(Instant.ofEpochMilli(1762965822812L)) // arbitrary date in Nov 2025
    def hours(n: Int): TimestampInterval = TimestampInterval.between(start, start.plusSecondsOption(n * 60 * 60).get)
    def days(n: Int): TimestampInterval = hours(n * 24)

  def getEphemerisCacheMisses(user: User, oid: Observation.Id, interval: TimestampInterval, force: Boolean = false) =
    withServices(user): svcs =>
      svcs.trackingService.asInstanceOf[TrackingService.Whitebox[IO]].getTrackingSnapshotEx(oid, interval, force).map: r =>
        r.toOption.get.base._2

  test("Identical requests should be fully cached."):
    for 
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      n1  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10), true), 24)
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10)), 0)
    yield ()

  test("Requests within the same day should be cached."):
    for 
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      n1  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10), true), 24)
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(20)), 0) 
    yield ()

  test("A longer ephemeris should have some cache misses."):
    for 
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      n1  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10), true), 24)
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(50)), 8) 
    yield ()

  test("A longer ephemeris should have some cache misses, but only once."):
    for 
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      n1  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10), true), 24)
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(50)), 8) 
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(50)), 0) 
    yield ()


