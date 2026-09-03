// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.CalibrationWorkType
import lucuma.odb.graphql.TestUsers
import lucuma.odb.util.Codecs.*
import org.typelevel.otel4s.trace.Tracer
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.Month
import java.time.ZoneId

// Integration tests for the multi-oid recalculation refactor and the startup
// reconciliation.
class CalibrationReconciliationSuite extends CalibrationCalcServiceSuiteSupport:

  override val pi = TestUsers.Standard.pi(1, 30)
  override val validUsers = List(pi)

  given Tracer[IO] = Tracer.noop

  private val when: Instant =
    LocalDateTime.of(2025, Month.MARCH, 3, 23, 30, 0)
      .atZone(ZoneId.of("America/Santiago")).toInstant

  private def countQueueInState(state: CalculationState): IO[Int] =
    withSession: session =>
      session.unique(
        sql"SELECT count(*) FROM t_calibration_calc WHERE c_state = $calculation_state".query(int8)
      )(state).map(_.toInt)

  private def countCalibrationObservations(pid: Program.Id): IO[Int] =
    withSession: session =>
      session.unique(
        sql"""
          SELECT count(*)
          FROM t_observation
          WHERE c_program_id = $program_id AND c_calibration_role IS NOT NULL
        """.query(int8)
      )(pid).map(_.toInt)

  private def startupDrain(batchSize: Int): IO[Unit] =
    withServicesResourceForObscalc(serviceUser): services =>
      CalibrationCalcDaemon.startupDrain[IO](batchSize, services)

  private def calibrationObs(pid: Program.Id): IO[List[(Observation.Id, CalibrationRole)]] =
    withSession: session =>
      session.execute(
        sql"""
          SELECT c_observation_id, c_calibration_role
          FROM t_observation
          WHERE c_program_id = $program_id AND c_calibration_role IS NOT NULL
          ORDER BY c_observation_id
        """.query(observation_id *: calibration_role)
      )(pid)

  private def asterismTargets(oid: Observation.Id): IO[List[Target.Id]] =
    withSession: session =>
      session.execute(
        sql"SELECT c_target_id FROM t_asterism_target WHERE c_observation_id = $observation_id"
          .query(target_id)
      )(oid)

  test("batched multi-oid recalculation matches per-oid recalculation"):
    for
      pid  <- createProgramAs(pi)
      t1   <- createTargetAs(pi, pid, "One")
      t2   <- createTargetAs(pi, pid, "Two")
      f2a  <- createFlamingos2LongSlitObservationAs(pi, pid, List(t1))
      f2b  <- createFlamingos2LongSlitObservationAs(pi, pid, List(t2))
      g1   <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, t1)
      g2   <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, t2)
      _    <- recalculateCalibrations(pid, when, f2a)
      _    <- recalculateCalibrations(pid, when, f2b)
      _    <- recalculateCalibrations(pid, when, g1)
      _    <- recalculateCalibrations(pid, when, g2)
      perOidCount <- countCalibrationObservations(pid)
      _    <- recalculateCalibrations(pid, when, NonEmptyList.of(f2a, f2b, g1, g2))
      batchCount  <- countCalibrationObservations(pid)
    yield
      assert(perOidCount > 0, "per-oid recalculation should create calibrations")
      assertEquals(batchCount, perOidCount)

  test("startup drain resets and empties a queue filled while the daemon was down"):
    for
      _      <- cleanup
      pid    <- createProgramAs(pi)
      t1     <- createTargetAs(pi, pid, "One")
      t2     <- createTargetAs(pi, pid, "Two")
      o1     <- createFlamingos2LongSlitObservationAs(pi, pid, List(t1))
      o2     <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, t2)
      _      <- runObscalcUpdate(pid, o1)
      _      <- runObscalcUpdate(pid, o2)
      now    <- timestampNow
      _      <- insertState(pid, o1, CalculationState.Pending, now)
      // A crash mid-calculation: the drain's reset must re-pend and process it.
      _      <- insertState(pid, o2, CalculationState.Calculating, now)
      _      <- startupDrain(batchSize = 10)
      s1     <- stateAndWorkType(o1)
      s2     <- stateAndWorkType(o2)
      left   <- countQueueInState(CalculationState.Pending)
      calibs <- countCalibrationObservations(pid)
    yield
      assertEquals(s1, Some((CalculationState.Ready, CalibrationWorkType.Recalc)))
      assertEquals(s2, Some((CalculationState.Ready, CalibrationWorkType.Recalc)))
      assertEquals(left, 0)
      assert(calibs > 0, "reconciliation should have created calibrations")

  test("startup drain retargets a calibration whose time changed while the daemon was down"):
    for
      _         <- cleanup
      pid       <- createProgramAs(pi)
      t1        <- createTargetAs(pi, pid, "One")
      o1        <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, t1)
      _         <- runObscalcUpdate(pid, o1)
      now       <- timestampNow
      _         <- insertState(pid, o1, CalculationState.Pending, now)
      _         <- startupDrain(batchSize = 10)
      calibs    <- calibrationObs(pid)
      cid       = calibs.collectFirst { case (oid, CalibrationRole.SpectroPhotometric) => oid }.get
      before    <- asterismTargets(cid)
      // The daemon is "down": the time edit only leaves a durable queue row.
      _         <- setObservationTimeAndDuration(pi, cid, Some(Timestamp.unsafeFromInstantTruncated(Instant.parse("2026-08-15T01:15:30Z"))), None)
      enqueued  <- stateAndWorkType(cid)
      _         <- startupDrain(batchSize = 10)
      after     <- stateAndWorkType(cid)
      targets   <- asterismTargets(cid)
      left      <- countQueueInState(CalculationState.Pending)
    yield
      assertEquals(enqueued, Some((CalculationState.Pending, CalibrationWorkType.Retarget)))
      assertEquals(after, Some((CalculationState.Ready, CalibrationWorkType.Retarget)))
      assertEquals(left, 0)
      assert(targets.nonEmpty, "the calibration should still have a target")
      assertNotEquals(targets, before, "the retarget should pick a different target for the new time")
