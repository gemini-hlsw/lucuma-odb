// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import lucuma.core.model.Observation
import lucuma.core.util.Timestamp
import lucuma.odb.data.PendingRecalc
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric.int4
import skunk.codec.text.text
import skunk.implicits.*

/**
 * Queue state machine for the durable calibration-recalculation work queue,
 */
trait CalibrationCalcService[F[_]]:

  /** Marks `calculating` entries `pending`. */
  def reset(using ServiceAccess, Transaction[F]): F[Unit]

  /**
   * loads up to `max` `pending`/`retry` entries (moving them to `calculating`),
   * ordered by `c_last_invalidation`, skipping rows locked by other workers.
   */
  def load(max: Int)(using ServiceAccess, Transaction[F]): F[List[PendingRecalc]]

  /** Claims the entry for `oid` if it is `pending`/`retry`. */
  def loadObs(
    oid: Observation.Id
  )(using ServiceAccess, Transaction[F]): F[Option[PendingRecalc]]

  def markReady(pending: PendingRecalc)(using ServiceAccess, Transaction[F]): F[Unit]

  def markRetry(oid: Observation.Id, error: String)(using ServiceAccess, Transaction[F]): F[Unit]

object CalibrationCalcService:

  def instantiate[F[_]: {Concurrent, Services}]: CalibrationCalcService[F] =
    new CalibrationCalcService[F]:

      override def reset(using ServiceAccess, Transaction[F]): F[Unit] =
        session.execute(Statements.ResetCalculating).void

      override def load(
        max: Int
      )(using ServiceAccess, Transaction[F]): F[List[PendingRecalc]] =
        session.execute(Statements.LoadPendingCalc)(max)

      override def loadObs(
        oid: Observation.Id
      )(using ServiceAccess, Transaction[F]): F[Option[PendingRecalc]] =
        session.option(Statements.LoadPendingCalcFor)(oid)

      override def markReady(
        pending: PendingRecalc
      )(using ServiceAccess, Transaction[F]): F[Unit] =
        for
          current <- session.option(Statements.SelectLastInvalidationForUpdate)(pending.observationId)
          _       <- current match
                       case Some(ts) if ts === pending.lastInvalidation =>
                         session.execute(Statements.MarkReady)(pending.observationId)
                       case _ =>
                         session.execute(Statements.MarkPending)(pending.observationId)
        yield ()

      override def markRetry(
        oid: Observation.Id,
        error: String
      )(using ServiceAccess, Transaction[F]): F[Unit] =
        session.execute(Statements.MarkRetry)((error, oid)).void

      object Statements:
        val pending: Codec[PendingRecalc] =
          (program_id *: observation_id *: core_timestamp).to[PendingRecalc]

        val ResetCalculating: Command[Void] =
          sql"""
            UPDATE t_calibration_calc
            SET
              c_state  = CASE
                WHEN c_retry_at IS NULL THEN 'pending' :: e_calculation_state
                ELSE 'retry' :: e_calculation_state
              END
            WHERE c_state = 'calculating'
          """.command

        val LoadPendingCalc: Query[Int, PendingRecalc] =
          sql"""
            WITH tasks AS (
              SELECT c_program_id, c_observation_id
              FROM t_calibration_calc
              WHERE (
                c_state = 'pending' OR
                (c_state = 'retry' AND c_retry_at <= now())
              )
              ORDER BY c_last_invalidation LIMIT $int4
              FOR UPDATE SKIP LOCKED
            )
            UPDATE t_calibration_calc c
            SET c_state = 'calculating'
            FROM tasks
            WHERE c.c_observation_id = tasks.c_observation_id
            RETURNING c.c_program_id, c.c_observation_id, c.c_last_invalidation
          """.query(pending)

        val LoadPendingCalcFor: Query[Observation.Id, PendingRecalc] =
          sql"""
            WITH task AS (
              SELECT c_program_id, c_observation_id
              FROM t_calibration_calc
              WHERE (
                c_state = 'pending' OR
                (c_state = 'retry' AND c_retry_at <= now())
              ) AND c_observation_id = $observation_id
              FOR UPDATE SKIP LOCKED
            )
            UPDATE t_calibration_calc c
            SET c_state = 'calculating'
            FROM task
            WHERE c.c_observation_id = task.c_observation_id
            RETURNING c.c_program_id, c.c_observation_id, c.c_last_invalidation
          """.query(pending)

        val SelectLastInvalidationForUpdate: Query[Observation.Id, Timestamp] =
          sql"""
            SELECT c_last_invalidation
            FROM t_calibration_calc
            WHERE c_observation_id = $observation_id
            FOR UPDATE
          """.query(core_timestamp)

        val MarkReady: Command[Observation.Id] =
          sql"""
            UPDATE t_calibration_calc
            SET c_state         = 'ready',
                c_last_update   = now(),
                c_failure_count = 0,
                c_retry_at      = NULL,
                c_error_message = NULL
            WHERE c_observation_id = $observation_id
          """.command

        val MarkPending: Command[Observation.Id] =
          sql"""
            UPDATE t_calibration_calc
            SET c_state         = 'pending',
                c_failure_count = 0,
                c_retry_at      = NULL,
                c_error_message = NULL
            WHERE c_observation_id = $observation_id
          """.command

        val MarkRetry: Command[(String, Observation.Id)] =
          sql"""
            UPDATE t_calibration_calc
            SET c_state         = 'retry',
                c_last_update   = now(),
                c_failure_count = c_failure_count + 1,
                c_retry_at      = now() + (interval '1 minute' * POWER(2, LEAST(c_failure_count, 5))),
                c_error_message = $text
            WHERE c_observation_id = $observation_id
          """.command
