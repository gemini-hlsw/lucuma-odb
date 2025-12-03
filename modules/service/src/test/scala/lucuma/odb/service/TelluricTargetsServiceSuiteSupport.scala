// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.either.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.TelluricTargets
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.json.tellurictype.transport.given
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.LocalDateTime

trait TelluricTargetsServiceSuiteSupport extends ExecutionTestSupportForGmos:

  // Service instantiation helpers (following ObscalcServiceSuiteSupport pattern)
  def withTelluricTargetsService[A](f: ServiceAccess ?=> TelluricTargetsService[IO] => IO[A]): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      f(services.telluricTargetsService)

  def withTelluricTargetsServiceTransactionally[A](f: (ServiceAccess, Transaction[IO]) ?=> TelluricTargetsService[IO] => IO[A]): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      services.transactionally:
        f(services.telluricTargetsService)

  // Service wrapper methods (use service methods)
  def reset: IO[Unit] =
    withTelluricTargetsServiceTransactionally(_.reset)

  def load(max: Int): IO[List[TelluricTargets.Pending]] =
    withTelluricTargetsServiceTransactionally(_.load(max))

  def loadObs(oid: Observation.Id): IO[Option[TelluricTargets.Pending]] =
    withTelluricTargetsServiceTransactionally(_.loadObs(oid))

  // Direct DB operations for test setup
  def insertPending(pending: TelluricTargets.Pending): IO[Unit] =
    withSession: session =>
      val ins: Command[(Program.Id, Observation.Id, Observation.Id, Timestamp, Int)] = sql"""
        INSERT INTO t_telluric_resolution (
          c_program_id,
          c_observation_id,
          c_science_observation_id,
          c_last_invalidation,
          c_failure_count,
          c_state
        ) VALUES (
          $program_id,
          $observation_id,
          $observation_id,
          $core_timestamp,
          $int4,
          'pending'::e_calculation_state
        )
        ON CONFLICT ON CONSTRAINT t_telluric_resolution_pkey DO UPDATE
          SET c_last_invalidation = $core_timestamp
      """.command.contramap((p, o, s, t, f) => (p, o, s, t, f, t))

      session.execute(ins)(
        pending.programId,
        pending.observationId,
        pending.scienceObservationId,
        pending.lastInvalidation,
        pending.failureCount
      ).void

  def insertMeta(meta: TelluricTargets.Meta): IO[Unit] =
    withSession: session =>
      val ins: Command[(Program.Id, Observation.Id, Observation.Id, CalculationState, Timestamp, Timestamp, Option[java.time.LocalDateTime], Int, Option[Target.Id], Option[String])] = sql"""
        INSERT INTO t_telluric_resolution (
          c_program_id,
          c_observation_id,
          c_science_observation_id,
          c_state,
          c_last_invalidation,
          c_last_update,
          c_retry_at,
          c_failure_count,
          c_resolved_target_id,
          c_error_message
        ) VALUES (
          $program_id,
          $observation_id,
          $observation_id,
          $calculation_state,
          $core_timestamp,
          $core_timestamp,
          ${timestamp.opt},
          $int4,
          ${target_id.opt},
          ${text.opt}
        )
        ON CONFLICT ON CONSTRAINT t_telluric_resolution_pkey DO UPDATE
          SET c_state = EXCLUDED.c_state,
              c_last_update = EXCLUDED.c_last_update,
              c_retry_at = EXCLUDED.c_retry_at,
              c_failure_count = EXCLUDED.c_failure_count,
              c_resolved_target_id = EXCLUDED.c_resolved_target_id,
              c_error_message = EXCLUDED.c_error_message
      """.command

      session.execute(ins)(
        meta.programId,
        meta.observationId,
        meta.scienceObservationId,
        meta.state,
        meta.lastInvalidation,
        meta.lastUpdate,
        meta.retryAt.map(_.toLocalDateTime),
        meta.failureCount,
        meta.resolvedTargetId,
        meta.errorMessage
      ).void

  def cleanup: IO[Unit] =
    withSession: session =>
      val truncate = sql"TRUNCATE t_telluric_resolution".command
      session.execute(truncate).void

  // Direct DB queries for verification
  def selectMeta(oid: Observation.Id): IO[Option[TelluricTargets.Meta]] =
    withSession: session =>
      val select: Query[Observation.Id, TelluricTargets.Meta] = sql"""
        SELECT c_observation_id,
               c_program_id,
               c_science_observation_id,
               c_state,
               c_last_invalidation,
               c_last_update,
               c_retry_at,
               c_failure_count,
               c_resolved_target_id,
               c_error_message
        FROM t_telluric_resolution
        WHERE c_observation_id = $observation_id
      """.query(
        observation_id *: program_id *: observation_id *: calculation_state *:
        timestamp *: timestamp *: timestamp.opt *: int4 *: target_id.opt *: text.opt
       ).map { case (oid, pid, scienceOid, state, lastInv, lastUpd, retryAt, failCount, targetId, errorMsg) =>
        TelluricTargets.Meta(oid, pid, scienceOid, state,
          Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv),
          Timestamp.fromLocalDateTimeTruncatedAndBounded(lastUpd),
          retryAt.map(t => Timestamp.fromLocalDateTimeTruncatedAndBounded(t)),
          failCount, targetId, errorMsg)
      }

      session.prepareR(select).use(_.option(oid))

  def selectAllMeta: IO[List[TelluricTargets.Meta]] =
    withSession: session =>
      val select: Query[Void, TelluricTargets.Meta] = sql"""
        SELECT c_observation_id,
               c_program_id,
               c_science_observation_id,
               c_state,
               c_last_invalidation,
               c_last_update,
               c_retry_at,
               c_failure_count,
               c_resolved_target_id,
               c_error_message
        FROM t_telluric_resolution
        ORDER BY c_last_invalidation
      """.query(
        observation_id *: program_id *: observation_id *: calculation_state *:
        timestamp *: timestamp *: timestamp.opt *: int4 *: target_id.opt *: text.opt
       ).map { case (oid, pid, scienceOid, state, lastInv, lastUpd, retryAt, failCount, targetId, errorMsg) =>
        TelluricTargets.Meta(oid, pid, scienceOid, state,
          Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv),
          Timestamp.fromLocalDateTimeTruncatedAndBounded(lastUpd),
          retryAt.map(t => Timestamp.fromLocalDateTimeTruncatedAndBounded(t)),
          failCount, targetId, errorMsg)
      }

      session.prepareR(select).use(_.stream(Void, 1024).compile.toList)

  def calculationState(oid: Observation.Id): IO[CalculationState] =
    withSession: session =>
      val query = sql"""
        SELECT c_state
        FROM t_telluric_resolution
        WHERE c_observation_id = $observation_id
      """.query(calculation_state)
      session.unique(query)(oid)

  // Test data factories (pure functions)
  def randomTime: Timestamp =
    Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().minusDays(1))

  def createPendingEntry(pid: Program.Id, oid: Observation.Id, sid: Observation.Id): TelluricTargets.Pending =
    TelluricTargets.Pending(
      observationId = oid,
      programId = pid,
      scienceObservationId = sid,
      lastInvalidation = randomTime,
      failureCount = 0
    )

  def createMetaEntry(pid: Program.Id, oid: Observation.Id, sid: Observation.Id, state: CalculationState): TelluricTargets.Meta =
    TelluricTargets.Meta(
      observationId = oid,
      programId = pid,
      scienceObservationId = sid,
      state = state,
      lastInvalidation = randomTime,
      lastUpdate = randomTime,
      retryAt = None,
      failureCount = 0,
      resolvedTargetId = None,
      errorMessage = None
    )

  // Convenience helpers
  def createTestProgram: IO[Program.Id] =
    createProgramAs(serviceUser, "Telluric Test Program")

  def createTestObservation(pid: Program.Id): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(serviceUser, pid, Nil)

  def createTelluricCalibrationObservation(pid: Program.Id): IO[Observation.Id] =
    for {
      oid <- createTestObservation(pid)
      _   <- setCalibrationRole(oid, CalibrationRole.Telluric)
    } yield oid

  def setCalibrationRole(oid: Observation.Id, role: CalibrationRole): IO[Unit] =
    withSession: session =>
      val update = sql"""
        UPDATE t_observation
        SET c_calibration_role = $calibration_role
        WHERE c_observation_id = $observation_id
      """.command
      session.execute(update)((role, oid)).void

  def createTestTarget(pid: Program.Id): IO[Target.Id] =
    createTargetAs(serviceUser, pid, "Test Target")

  def createF2Observation(pid: Program.Id): IO[Observation.Id] =
    createFlamingos2LongSlitObservationAs(serviceUser, pid, Nil)

  def setObservationDuration(oid: Observation.Id, durationSeconds: Long): IO[Unit] =
    withSession: session =>
      val update = sql"""
        UPDATE t_observation
        SET c_observation_duration = make_interval(secs => $int8)
        WHERE c_observation_id = $observation_id
      """.command
      session.execute(update)((durationSeconds, oid)).void

  // Direct DB query for F2 config (mirrors the query in TelluricTargetsService)
  def selectF2Config(oid: Observation.Id): IO[Option[(lucuma.core.model.TelluricType, Long)]] =
    withSession: session =>
      val query: Query[Observation.Id, (lucuma.core.model.TelluricType, Long)] = sql"""
        SELECT f2.c_telluric_type, EXTRACT(EPOCH FROM o.c_observation_duration)::bigint * 1000000
        FROM   t_flamingos_2_long_slit f2
        JOIN   t_observation o ON o.c_observation_id = f2.c_observation_id
        WHERE  f2.c_observation_id = $observation_id
          AND  o.c_observation_duration IS NOT NULL
      """.query(jsonb.emap(_.as[lucuma.core.model.TelluricType].leftMap(_.getMessage)) *: int8)
      session.prepareR(query).use(_.option(oid))
