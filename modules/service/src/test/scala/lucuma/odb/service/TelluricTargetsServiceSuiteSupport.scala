// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.TelluricTargets
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

trait TelluricTargetsServiceSuiteSupport extends ExecutionTestSupportForGmos:

  def withTelluricTargetsServiceTransactionally[A](
    f: (ServiceAccess, Transaction[IO]) ?=> TelluricTargetsService[IO] => IO[A]
  ): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      services.transactionally:
        f(services.telluricTargetsService)

  def reset: IO[Unit] =
    withTelluricTargetsServiceTransactionally(_.reset)

  def load(max: Int): IO[List[TelluricTargets.Pending]] =
    withTelluricTargetsServiceTransactionally(_.load(max))

  def loadObs(oid: Observation.Id): IO[Option[TelluricTargets.Pending]] =
    withTelluricTargetsServiceTransactionally(_.loadObs(oid))

  def insertPending(pending: TelluricTargets.Pending): IO[Unit] =
    withSession: session =>
      val ins: Command[(Program.Id, Observation.Id, Observation.Id, Timestamp, Int)] =
        sql"""
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
      val ins: Command[(Program.Id, Observation.Id, Observation.Id, CalculationState, Timestamp, Timestamp, Option[Timestamp], Int, Option[Target.Id], Option[String])] =
        sql"""
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
                ${core_timestamp.opt},
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
        meta.retryAt,
        meta.failureCount,
        meta.resolvedTargetId,
        meta.errorMessage
      ).void

  val cleanup: IO[Unit] =
    withSession: session =>
      val truncate = sql"TRUNCATE t_telluric_resolution".command
      session.execute(truncate).void

  // Codec for Meta matching the service
  private val metaCodec: Codec[TelluricTargets.Meta] =
    (observation_id *: program_id *: observation_id *: calculation_state *:
     core_timestamp *: core_timestamp *: core_timestamp.opt *: int4 *:
     target_id.opt *: text.opt *: time_span).to[TelluricTargets.Meta]

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
               c_error_message,
               c_science_duration
        FROM t_telluric_resolution
        WHERE c_observation_id = $observation_id
      """.query(metaCodec)

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
               c_error_message,
               c_science_duration
        FROM t_telluric_resolution
        ORDER BY c_last_invalidation
      """.query(metaCodec)

      session.prepareR(select).use(_.stream(Void, 1024).compile.toList)

  def calculationState(oid: Observation.Id): IO[CalculationState] =
    withSession: session =>
      val query = sql"""
        SELECT c_state
        FROM t_telluric_resolution
        WHERE c_observation_id = $observation_id
      """.query(calculation_state)
      session.unique(query)(oid)

  // Test data factories
  val randomTime: Timestamp =
    Timestamp.unsafeFromInstantTruncated(java.time.Instant.now)

  def createPendingEntry(pid: Program.Id, oid: Observation.Id, sid: Observation.Id): TelluricTargets.Pending =
    import lucuma.core.syntax.timespan.*
    TelluricTargets.Pending(
      observationId = oid,
      programId = pid,
      scienceObservationId = sid,
      lastInvalidation = randomTime,
      failureCount = 0,
      scienceDuration = 1.hourTimeSpan
    )

  def createMetaEntry(pid: Program.Id, oid: Observation.Id, sid: Observation.Id, state: CalculationState): TelluricTargets.Meta =
    import lucuma.core.syntax.timespan.*
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
      errorMessage = None,
      scienceDuration = 1.hourTimeSpan
    )

  // Convenience helpers
  def createTelluricCalibrationObservation(user: User, pid: Program.Id): IO[Observation.Id] =
    for {
      oid <- createFlamingos2LongSlitObservationAs(user, pid, Nil)
      _   <- setObservationCalibrationRole(List(oid), CalibrationRole.Telluric)
    } yield oid
