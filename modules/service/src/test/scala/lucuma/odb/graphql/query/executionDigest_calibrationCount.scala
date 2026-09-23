// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.itc.client.SpectroscopyInput
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.refined.*

import java.time.Instant

class executionDigest_calibrationCount
  extends OdbSuite
  with ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport:

  // Vary the science time with the requested exposure time and count.
  override def fakeItcSpectroscopyResultFor(input: SpectroscopyInput): Option[IntegrationTime] =
    input.parameters.mode.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(time, count, _) => IntegrationTime(time, count).some
      case ExposureTimeMode.SignalToNoiseMode(_, _)          => IntegrationTime(5.minuteTimeSpan, 4.refined).some

  def calibrationCount(pid: Program.Id, oid: Observation.Id): IO[Int] =
    runObscalcUpdate(pid, oid) *>
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            execution { digest { value { estimate { calibrationCount } } } }
          }
        }
      """
    ).map: json =>
      json.hcursor.downFields("observation", "execution", "digest", "value", "estimate", "calibrationCount").require[Int]

  private def setTelluricType(oid: Observation.Id, tag: String): IO[Unit] =
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: { observingMode: { flamingos2LongSlit: { telluricType: { tag: $tag } } } }
        }) {
          observations { id }
        }
      }"""
    ).void

  test("one epoch for short science, two for long, unchanged by NO_TELLURIC"):
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
      (1, 2, 2)
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
