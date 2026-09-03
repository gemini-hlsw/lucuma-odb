// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.CalibrationWorkType
import lucuma.odb.data.PendingRecalc
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

trait CalibrationCalcServiceSuiteSupport extends ExecutionTestSupportForFlamingos2:

  def withCalibrationCalcServiceTransactionally[A](
    f: (ServiceAccess, Transaction[IO]) ?=> CalibrationCalcService[IO] => IO[A]
  ): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      services.transactionally:
        f(services.calibrationCalcService)

  def reset: IO[Unit] =
    withCalibrationCalcServiceTransactionally(_.reset)

  def load(max: Int): IO[List[PendingRecalc]] =
    withCalibrationCalcServiceTransactionally(_.load(max))

  def loadObs(oid: Observation.Id): IO[Option[PendingRecalc]] =
    withCalibrationCalcServiceTransactionally(_.loadObs(oid))

  def markReady(pending: PendingRecalc): IO[Unit] =
    withCalibrationCalcServiceTransactionally(_.markReady(pending))

  def markRetry(oid: Observation.Id, error: String): IO[Unit] =
    withCalibrationCalcServiceTransactionally(_.markRetry(oid, error))

  val cleanup: IO[Unit] =
    withSession: session =>
      session.execute(sql"TRUNCATE t_calibration_calc".command).void

  // Insert a 'pending' row (the state the cascade trigger would produce).
  def insertPending(
    pid:              Program.Id,
    oid:              Observation.Id,
    lastInvalidation: Timestamp,
    workType:         CalibrationWorkType = CalibrationWorkType.Recalc
  ): IO[Unit] =
    withSession: session =>
      session.execute(
        sql"""
          INSERT INTO t_calibration_calc (c_program_id, c_observation_id, c_state, c_last_invalidation, c_work_type)
          VALUES ($program_id, $observation_id, 'pending', $core_timestamp, $calibration_work_type)
          ON CONFLICT (c_observation_id) DO UPDATE
            SET c_state = EXCLUDED.c_state,
                c_last_invalidation = EXCLUDED.c_last_invalidation,
                c_retry_at = NULL,
                c_failure_count = 0,
                c_error_message = NULL
        """.command
      )(pid, oid, lastInvalidation, workType).void

  def stateAndWorkType(oid: Observation.Id): IO[Option[(CalculationState, CalibrationWorkType)]] =
    withSession: session =>
      session.option(
        sql"""
          SELECT c_state, c_work_type FROM t_calibration_calc
          WHERE c_observation_id = $observation_id
        """.query(calculation_state *: calibration_work_type)
      )(oid)

  def insertState(
    pid:              Program.Id,
    oid:              Observation.Id,
    state:            CalculationState,
    lastInvalidation: Timestamp,
    retryAt:          Option[Timestamp] = None,
    failureCount:     Int               = 0
  ): IO[Unit] =
    withSession: session =>
      session.execute(
        sql"""
          INSERT INTO t_calibration_calc (
            c_program_id, c_observation_id, c_state,
            c_last_invalidation, c_retry_at, c_failure_count
          ) VALUES (
            $program_id, $observation_id, $calculation_state,
            $core_timestamp, ${core_timestamp.opt}, $int4
          )
          ON CONFLICT (c_observation_id) DO UPDATE
            SET c_state = EXCLUDED.c_state,
                c_last_invalidation = EXCLUDED.c_last_invalidation,
                c_retry_at = EXCLUDED.c_retry_at,
                c_failure_count = EXCLUDED.c_failure_count
        """.command
      )(pid, oid, state, lastInvalidation, retryAt, failureCount).void

  def calculationState(oid: Observation.Id): IO[CalculationState] =
    withSession: session =>
      session.unique(
        sql"SELECT c_state FROM t_calibration_calc WHERE c_observation_id = $observation_id"
          .query(calculation_state)
      )(oid)

  def selectRow(oid: Observation.Id): IO[Option[(CalculationState, Timestamp, Option[Timestamp], Int, Option[String])]] =
    withSession: session =>
      session.option(
        sql"""
          SELECT c_state, c_last_invalidation, c_retry_at, c_failure_count, c_error_message
          FROM t_calibration_calc
          WHERE c_observation_id = $observation_id
        """.query(calculation_state *: core_timestamp *: core_timestamp.opt *: int4 *: text.opt)
      )(oid)

  def rowCount: IO[Int] =
    withSession: session =>
      session.unique(sql"SELECT count(*) FROM t_calibration_calc".query(skunk.codec.numeric.int8)).map(_.toInt)
