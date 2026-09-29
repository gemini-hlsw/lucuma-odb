// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState.Inactive
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.odb.graphql.subscription.SubscriptionUtils

import java.time.Instant

// Each telluric the calibration count predicts but that is not in the group
// yet costs the 15-minute placeholder, and the total carries it.
class executionDigest_expectedCalibrations
  extends OdbSuite
  with ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport
  with CalibrationCountTestSupport
  with SubscriptionUtils:

  private val when: Instant = Instant.parse("2024-01-01T12:00:00Z")

  // Program-charged microseconds of the estimate's parts.
  private case class Estimate(count: Int, expected: Long, total: Long, scienceAndSetups: Long):
    def expectedMinutes: Long = expected / 60_000_000L
    def restMinutes: Long     = (total - scienceAndSetups) / 60_000_000L

  private def estimate(pid: Program.Id, oid: Observation.Id): IO[Estimate] =
    runObscalcUpdate(pid, oid) *>
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              digest {
                value {
                  estimate {
                    calibrationCount
                    setupCount
                    setup { full { microseconds } }
                    science { program { microseconds } }
                    expectedCalibrations { program { microseconds } }
                    total { program { microseconds } }
                  }
                }
              }
            }
          }
        }
      """
    ).map: json =>
      val est    = json.hcursor
                     .downFields("observation", "execution", "digest", "value", "estimate")
      val count  = est.downField("calibrationCount").require[Int]
      val setups = est.downField("setupCount").require[Int]
      val setup  = est.downFields("setup", "full", "microseconds").require[Long]
      val sci    = est.downFields("science", "program", "microseconds").require[Long]
      val exp    = est.downFields("expectedCalibrations", "program", "microseconds").require[Long]
      val total  = est.downFields("total", "program", "microseconds").require[Long]
      Estimate(count, exp, total, sci + setup * setups)

  private def telluricsOf(oid: Observation.Id): IO[List[Observation.Id]] =
    queryObservation(oid).flatMap: obs =>
      obs.groupId.fold(IO.pure(List.empty)): gid =>
        queryObservationsInGroup(gid).map: obs =>
          obs.filter(_.calibrationRole.contains(CalibrationRole.Telluric)).map(_.id)

  test("every predicted telluric is charged: the placeholder, then the tellurics' average"):
    for
      p         <- createProgramAs(pi)
      t         <- createTargetWithProfileAs(pi, p)
      o         <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _         <- setExposureTime(o, 240)
      e1        <- estimate(p, o)
      _         <- recalculateCalibrations(p, when, o)
      tellurics <- telluricsOf(o)
      e2        <- estimate(p, o)
      _         <- sleep >> resolveTelluricTargets
      totals    <- tellurics.traverse(estimate(p, _)).map(_.map(_.total))
      e3        <- estimate(p, o)
    yield
      // Four hours of science: three sets.  None materialised, then a long
      // visit's pair with no digest yet, then the pair with digests.
      assertEquals(e1.count, 3)
      assertEquals(e1.expectedMinutes, 45L)
      assertEquals(e1.restMinutes, 45L)
      assertEquals(tellurics.size, 2)
      assertEquals(e2.expectedMinutes, 15L)
      assertEquals(e2.restMinutes, 15L)
      assertEquals(e3.expected, totals.sum / totals.size)
      assertEquals(e3.total - e3.scienceAndSetups, e3.expected)

  test("a declined telluric means no expected calibrations"):
    for
      p            <- createProgramAs(pi)
      t            <- createTargetWithProfileAs(pi, p)
      o            <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _            <- setExposureTime(o, 240)
      _            <- recalculateCalibrations(p, when, o)
      tellurics    <- telluricsOf(o)
      _            <- setObservationWorkflowState(pi, tellurics.head, Inactive)
      e            <- estimate(p, o)
    yield
      assertEquals(e.count, 3)
      assertEquals(e.expected, 0L)
      assertEquals(e.restMinutes, 0L)

  test("no telluric type means no expected calibrations, the count stays"):
    for
      p            <- createProgramAs(pi)
      t            <- createTargetWithProfileAs(pi, p)
      o            <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _            <- setExposureTime(o, 240)
      _            <- setTelluricType(o, "NO_TELLURIC")
      e            <- estimate(p, o)
    yield
      assertEquals(e.count, 3)
      assertEquals(e.expected, 0L)
      assertEquals(e.restMinutes, 0L)

  test("a telluric's own digest expects nothing"):
    for
      p         <- createProgramAs(pi)
      t         <- createTargetWithProfileAs(pi, p)
      o         <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _         <- runObscalcUpdate(p, o)
      _         <- recalculateCalibrations(p, when, o)
      tellurics <- telluricsOf(o)
      e         <- estimate(p, tellurics.head)
    yield assertEquals(e.expected, 0L)
