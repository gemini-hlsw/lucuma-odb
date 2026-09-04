// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.util.CalculationState
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

// Tests the `cascade_calibration_invalidation` trigger and the
// `invalidate_calibration_calc` procedure.
class CalibrationCalcTriggerSuite extends ExecutionTestSupportForFlamingos2 {

  override val pi = TestUsers.Standard.pi(1, 30)
  override val validUsers = List(pi)

  private def seedObscalc(oid: Observation.Id): IO[Unit] =
    withSession: session =>
      session.execute(
        sql"""
          INSERT INTO t_obscalc (c_program_id, c_observation_id, c_obscalc_state, c_last_update)
          SELECT c_program_id, c_observation_id, 'pending', timestamp '2000-01-01'
          FROM t_observation
          WHERE c_observation_id = $observation_id
          ON CONFLICT (c_program_id, c_observation_id) DO UPDATE
            SET c_obscalc_state = 'pending',
                c_last_update   = timestamp '2000-01-01'
        """.command
      )(oid).void

  private def settleObscalcToReady(oid: Observation.Id): IO[Unit] =
    withSession: session =>
      session.execute(
        sql"""
          UPDATE t_obscalc
          SET c_obscalc_state = 'ready', c_last_update = now()
          WHERE c_observation_id = $observation_id
        """.command
      )(oid).void

  private def queueState(oid: Observation.Id): IO[Option[CalculationState]] =
    withSession: session =>
      session.option(
        sql"SELECT c_state FROM t_calibration_calc WHERE c_observation_id = $observation_id"
          .query(calculation_state)
      )(oid)

  private def cleanupQueue: IO[Unit] =
    withSession: session =>
      session.execute(sql"TRUNCATE t_calibration_calc".command).void

  test("science obs settling to ready enqueues a pending calibration recalculation"):
    for
      _     <- cleanupQueue
      pid   <- createProgramAs(pi, "Trigger Test")
      oid   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      _     <- seedObscalc(oid)
      _     <- settleObscalcToReady(oid)
      state <- queueState(oid)
    yield assertEquals(state, Some(CalculationState.Pending))

  test("calibration obs settling to ready does NOT enqueue"):
    for
      _     <- cleanupQueue
      pid   <- createProgramAs(pi, "Trigger Test")
      oid   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      _     <- setObservationCalibrationRole(List(oid), CalibrationRole.Telluric)
      _     <- seedObscalc(oid)
      _     <- settleObscalcToReady(oid)
      state <- queueState(oid)
    yield assertEquals(state, None)

  test("re-invalidation moves a ready queue row back to pending"):
    for
      _      <- cleanupQueue
      pid    <- createProgramAs(pi, "Trigger Test")
      oid    <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      _      <- seedObscalc(oid)
      _      <- settleObscalcToReady(oid)
      first  <- queueState(oid)
      // Simulate the daemon completing this row, then a fresh obscalc settle.
      _      <- withSession: session =>
                  session.execute(
                    sql"UPDATE t_calibration_calc SET c_state = 'ready' WHERE c_observation_id = $observation_id".command
                  )(oid)
      _      <- settleObscalcToReady(oid)
      second <- queueState(oid)
    yield
      assertEquals(first, Some(CalculationState.Pending))
      assertEquals(second, Some(CalculationState.Pending))

  test("hard-deleting a science obs cascades to remove its queue row"):
    for
      _     <- cleanupQueue
      pid   <- createProgramAs(pi, "Trigger Test")
      oid   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      _     <- seedObscalc(oid)
      _     <- settleObscalcToReady(oid)
      _     <- queueState(oid)
      // Purely the FK ON DELETE CASCADE: deleting t_obscalc rows fires no trigger.
      _     <- withSession: session =>
                 session.execute(sql"DELETE FROM t_observation WHERE c_observation_id = $observation_id".command)(oid)
      state <- queueState(oid)
    yield assertEquals(state, None)
}
