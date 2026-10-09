// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport

import java.time.Instant

class executionDigest_calibrationCount
  extends OdbSuite
  with ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport
  with CalibrationCountTestSupport:

  test("one set for short science, two for long, none with NO_TELLURIC"):
    assertIO(
      for
        p  <- createProgramAs(pi)
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        _  <- setExposureTime(o, 60)
        c1 <- calibrationCount(p, o)
        _  <- setExposureTime(o, 120)
        c2 <- calibrationCount(p, o)
        _  <- setTelluricType(o, "NO_TELLURIC")
        c0 <- calibrationCount(p, o)
      yield (c1, c2, c0),
      (1, 2, 0)
    )

  test("the flats and arcs the sequence shows do not count as science"):
    // Four 1298 second exposures make about 89 minutes of science: under the 90
    // minute interval on its own, past it with the opening set of flats and arcs.
    assertIO(
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        _ <- query(
               pi,
               s"""mutation {
                 updateObservations(input: {
                   WHERE: { id: { EQ: "$o" } }
                   SET: {
                     observingMode: {
                       flamingos2LongSlit: {
                         exposureTimeMode: {
                           timeAndCount: { time: { seconds: 1298 }, count: 4, at: { nanometers: 1390 } }
                         }
                       }
                     }
                   }
                 }) {
                   observations { id }
                 }
               }"""
             )
        c <- calibrationCount(p, o)
        m <- scienceMinutes(o)
      yield
        // Preconditions: without them the count of 1 would prove nothing.
        assert(m.steps < 90, s"science steps should be under 90 minutes, got ${m.steps}")
        assert(m.total > 90, s"science with its flats and arcs should pass 90 minutes, got ${m.total}")
        c,
      1
    )

  private case class ScienceMinutes(total: BigDecimal, steps: BigDecimal)

  private def scienceMinutes(oid: Observation.Id): IO[ScienceMinutes] =
    query(
      pi,
      s"""query {
        observation(observationId: "$oid") {
          execution { digest { value { science {
            timeEstimate { total { minutes } }
            steps { science { time { total { minutes } } } }
          } } } }
        }
      }"""
    ).map: json =>
      val sci = json.hcursor.downFields("observation", "execution", "digest", "value", "science")
      ScienceMinutes(
        sci.downFields("timeEstimate", "total", "minutes").require[BigDecimal],
        sci.downFields("steps", "science", "time", "total", "minutes").require[BigDecimal]
      )

  test("a mode that takes no telluric reports 0"):
    assertIO(
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2ImagingObservationAs(pi, p, t)
        c <- calibrationCount(p, o)
      yield c,
      0
    )

  test("a telluric's own digest reports 0"):
    assertIO(
      for
        p   <- createProgramAs(pi)
        t   <- createTargetWithProfileAs(pi, p)
        o   <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        _   <- runObscalcUpdate(p, o)
        _   <- recalculateCalibrations(p, Instant.parse("2024-01-01T12:00:00Z"), o)
        obs <- queryObservation(o)
        tel <- queryObservationsInGroup(obs.groupId.get).map(_.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id)
        c   <- calibrationCount(p, tel)
      yield c,
      0
    )
