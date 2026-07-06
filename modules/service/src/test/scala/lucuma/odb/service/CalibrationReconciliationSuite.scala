// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.CalculationState
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2
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
class CalibrationReconciliationSuite extends ExecutionTestSupportForFlamingos2 {

  override val pi = TestUsers.Standard.pi(1, 30)
  override val validUsers = List(pi)

  given Tracer[IO] = Tracer.noop

  private val when: Instant =
    LocalDateTime.of(2025, Month.MARCH, 3, 23, 30, 0)
      .atZone(ZoneId.of("America/Santiago")).toInstant

  private def cleanupQueue: IO[Unit] =
    withSession: session =>
      session.execute(sql"TRUNCATE t_calibration_calc".command).void

  private def insertState(
    pid:   Program.Id,
    oid:   Observation.Id,
    state: CalculationState
  ): IO[Unit] =
    withSession: session =>
      session.execute(
        sql"""
          INSERT INTO t_calibration_calc (c_program_id, c_observation_id, c_state)
          VALUES ($program_id, $observation_id, $calculation_state)
          ON CONFLICT (c_observation_id) DO UPDATE
            SET c_state = EXCLUDED.c_state, c_retry_at = NULL, c_failure_count = 0
        """.command
      )(pid, oid, state).void

  private def queueState(oid: Observation.Id): IO[Option[CalculationState]] =
    withSession: session =>
      session.option(
        sql"SELECT c_state FROM t_calibration_calc WHERE c_observation_id = $observation_id"
          .query(calculation_state)
      )(oid)

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
      _      <- cleanupQueue
      pid    <- createProgramAs(pi)
      t1     <- createTargetAs(pi, pid, "One")
      t2     <- createTargetAs(pi, pid, "Two")
      o1     <- createFlamingos2LongSlitObservationAs(pi, pid, List(t1))
      o2     <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, t2)
      _      <- runObscalcUpdate(pid, o1)
      _      <- runObscalcUpdate(pid, o2)
      _      <- insertState(pid, o1, CalculationState.Pending)
      // A crash mid-calculation: the drain's reset must re-pend and process it.
      _      <- insertState(pid, o2, CalculationState.Calculating)
      _      <- startupDrain(batchSize = 10)
      s1     <- queueState(o1)
      s2     <- queueState(o2)
      left   <- countQueueInState(CalculationState.Pending)
      calibs <- countCalibrationObservations(pid)
    yield
      assertEquals(s1, Some(CalculationState.Ready))
      assertEquals(s2, Some(CalculationState.Ready))
      assertEquals(left, 0)
      assert(calibs > 0, "reconciliation should have created calibrations")
}
