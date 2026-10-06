// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState.Inactive
import lucuma.core.enums.SlewStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.odb.graphql.subscription.SubscriptionUtils
import lucuma.odb.util.Codecs.observation_id
import skunk.codec.boolean.bool
import skunk.codec.text.text
import skunk.syntax.all.*

import java.time.Instant

// The estimate splits the calibration count into the tellurics already in the
// group, each at its own estimate, and those still expected, each at the
// group's average or the 15-minute placeholder.  Only the expected time joins
// the total.
class executionDigest_calibrationEstimate
  extends OdbSuite
  with ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport
  with CalibrationCountTestSupport
  with SubscriptionUtils:

  private val when: Instant = Instant.parse("2024-01-01T12:00:00Z")

  // Program-charged microseconds of the estimate's parts.
  private case class Estimate(
    count:            Int,
    existingCount:    Int,
    existing:         Long,
    expectedCount:    Int,
    expected:         Long,
    total:            Long,
    scienceAndSetups: Long
  ):
    def existingMinutes: Long = existing / 60_000_000L
    def expectedMinutes: Long = expected / 60_000_000L
    def restMinutes: Long     = (total - scienceAndSetups) / 60_000_000L

  private def estimate(pid: Program.Id, oid: Observation.Id): IO[Estimate] =
    runObscalcUpdate(pid, oid) *> storedEstimate(oid)

  // What the stored digest says, without recalculating it.
  private def storedEstimate(oid: Observation.Id): IO[Estimate] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              digest {
                value {
                  estimate {
                    setupCount
                    setup { full { microseconds } }
                    science { program { microseconds } }
                    calibrations {
                      count
                      existing { count time { program { microseconds } } }
                      expected { count time { program { microseconds } } }
                    }
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
      val count  = est.downFields("calibrations", "count").require[Int]
      val setups = est.downField("setupCount").require[Int]
      val setup  = est.downFields("setup", "full", "microseconds").require[Long]
      val sci    = est.downFields("science", "program", "microseconds").require[Long]
      val cal    = est.downField("calibrations")
      val existN = cal.downFields("existing", "count").require[Int]
      val exist  = cal.downFields("existing", "time", "program", "microseconds").require[Long]
      val expN   = cal.downFields("expected", "count").require[Int]
      val exp    = cal.downFields("expected", "time", "program", "microseconds").require[Long]
      val total  = est.downFields("total", "program", "microseconds").require[Long]
      Estimate(count, existN, exist, expN, exp, total, sci + setup * setups)

  private def obscalcStateAndStale(oid: Observation.Id): IO[(String, Boolean)] =
    withSession: s =>
      s.unique(
        sql"""
          SELECT c_obscalc_state::text, c_calibrations_stale
          FROM   t_obscalc
          WHERE  c_observation_id = $observation_id
        """.query(text *: bool)
      )(oid)

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
      _         <- setExposureTime(o, 240, 12)
      e1        <- estimate(p, o)
      _         <- recalculateCalibrations(p, when, o)
      tellurics <- telluricsOf(o)
      e2        <- estimate(p, o)
      _         <- sleep >> resolveTelluricTargets
      totals    <- tellurics.traverse(estimate(p, _)).map(_.map(_.total))
      e3        <- estimate(p, o)
    yield
      // Four hours of science in 20-minute exposures, so the ABBA cycle stays
      // under the limit: three sets.  None materialised, then a long visit's
      // pair with no digest yet, then the pair with digests.
      assertEquals(e1.count, 3)
      assertEquals(e1.existingCount, 0)
      assertEquals(e1.existing, 0L)
      assertEquals(e1.expectedCount, 3)
      assertEquals(e1.expectedMinutes, 45L)
      assertEquals(e1.restMinutes, 45L)
      assertEquals(tellurics.size, 2)
      assertEquals(e2.existingCount, 2)
      assertEquals(e2.existingMinutes, 30L)
      assertEquals(e2.expectedCount, 1)
      assertEquals(e2.expectedMinutes, 15L)
      assertEquals(e2.restMinutes, 15L)
      assertEquals(e3.existingCount, 2)
      assertEquals(e3.existing, totals.sum)
      assertEquals(e3.expectedCount, 1)
      assertEquals(e3.expected, totals.sum / totals.size)
      assertEquals(e3.total - e3.scienceAndSetups, e3.expected)

  test("a spent telluric feeds the average at its original estimate, not what is left of it"):
    for
      p         <- createProgramAs(pi)
      t         <- createTargetWithProfileAs(pi, p)
      o         <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _         <- setExposureTime(o, 240, 12)
      _         <- recalculateCalibrations(p, when, o)
      tellurics <- telluricsOf(o)
      _         <- sleep >> resolveTelluricTargets
      totals    <- tellurics.traverse(estimate(p, _)).map(_.map(_.total))
      _         <- recordVisitAs(serviceUser, tellurics.head)
      _         <- declareCompleteDirectly(tellurics.head)
      spent     <- estimate(p, tellurics.head)
      e         <- estimate(p, o)
    yield
      assertEquals(spent.total, 0L)
      assertEquals(e.existingCount, 1)
      assertEquals(e.existing, totals(1))
      assertEquals(e.expectedCount, 2)
      assertEquals(e.expected, (totals.sum / 2) * 2)

  test("a telluric with only a slew visit is not spent"):
    for
      p         <- createProgramAs(pi)
      t         <- createTargetWithProfileAs(pi, p)
      o         <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _         <- setExposureTime(o, 240, 12)
      _         <- recalculateCalibrations(p, when, o)
      tellurics <- telluricsOf(o)
      _         <- sleep >> resolveTelluricTargets
      totals    <- tellurics.traverse(estimate(p, _)).map(_.map(_.total))
      _         <- addSlewEventAs(serviceUser, tellurics.head, SlewStage.StartSlew)
      e         <- estimate(p, o)
    yield
      assertEquals(e.existingCount, 2)
      assertEquals(e.existing, totals.sum)
      assertEquals(e.expectedCount, 1)
      assertEquals(e.expected, totals.sum / totals.size)

  test("a telluric change marks the science's estimate stale, and a refresh updates it"):
    for
      p         <- createProgramAs(pi)
      t         <- createTargetWithProfileAs(pi, p)
      o         <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _         <- setExposureTime(o, 240, 12)
      _         <- recalculateCalibrations(p, when, o)
      tellurics <- telluricsOf(o)
      _         <- runObscalcUpdate(p, o) *> refreshCalibrationsAs(serviceUser)
      settled   <- obscalcStateAndStale(o)
      _         <- setObservationWorkflowState(pi, tellurics.head, Inactive)
      declined  <- obscalcStateAndStale(o)
      stale     <- storedEstimate(o)
      _         <- refreshCalibrationsAs(serviceUser)
      refreshed <- obscalcStateAndStale(o)
      e         <- storedEstimate(o)
    yield
      // Declining marks the estimate stale but keeps the digest ready, so the
      // calibrations service still sees the science as active.
      assertEquals(settled, ("ready", false))
      assertEquals(declined, ("ready", true))
      assertEquals(stale.existingCount, 2)
      assertEquals(refreshed, ("ready", false))
      assertEquals(e.count, 2)
      assertEquals(e.existingCount, 1)
      assertEquals(e.expectedCount, 1)

  test("a declined telluric fills its slot at no cost, the later ones are still expected"):
    for
      p            <- createProgramAs(pi)
      t            <- createTargetWithProfileAs(pi, p)
      o            <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _            <- setExposureTime(o, 240, 12)
      _            <- recalculateCalibrations(p, when, o)
      tellurics    <- telluricsOf(o)
      _            <- setObservationWorkflowState(pi, tellurics.head, Inactive)
      e            <- estimate(p, o)
    yield
      // Three sets and a long visit's pair, one of them declined.
      assertEquals(e.count, 2)
      assertEquals(e.existingCount, 1)
      assertEquals(e.expectedCount, 1)
      assertEquals(e.expectedMinutes, 15L)
      assertEquals(e.restMinutes, 15L)

  test("no telluric type means no calibrations at all"):
    for
      p            <- createProgramAs(pi)
      t            <- createTargetWithProfileAs(pi, p)
      o            <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _            <- setExposureTime(o, 240, 12)
      _            <- setTelluricType(o, "NO_TELLURIC")
      e            <- estimate(p, o)
    yield
      assertEquals(e.count, 0)
      assertEquals(e.existingCount, 0)
      assertEquals(e.expectedCount, 0)
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
    yield
      assertEquals(e.existingCount, 0)
      assertEquals(e.expectedCount, 0)
      assertEquals(e.expected, 0L)
