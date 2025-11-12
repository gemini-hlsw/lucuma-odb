// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.catalog.telluric.TelluricClient
import lucuma.catalog.telluric.TelluricSearchInput
import lucuma.catalog.telluric.TelluricSearchInput.*
import lucuma.catalog.telluric.TelluricStar
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.TelluricResolution
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.LocalDateTime
import scala.concurrent.duration.*

class TelluricResolutionServiceSuite extends ExecutionTestSupportForGmos {

  override val serviceUser = TestUsers.service(nextId)
  override val validUsers = List(serviceUser)

  // Mock TelluricClient for testing
  class MockTelluricClient(
    shouldSucceed: Boolean = true,
    shouldReturnTarget: Boolean = true,
    delayMs: Long = 0
  ) extends TelluricClient[IO] {
    override def search(input: TelluricSearchInput): IO[List[TelluricStar]] = {
      if (delayMs > 0) IO.sleep(delayMs.millis) *> {
        if (shouldSucceed) {
          if (shouldReturnTarget) {
            // Return a mock telluric star
            val mockStar = TelluricStar(
              hip = 12345,
              spType = input.spType.toInput,
              coordinates = input.coordinates,
              distance = 100.0,
              hmag = 8.0,
              score = 0.95,
              order = TelluricCalibrationOrder.Before
            )
            List(mockStar).pure[IO]
          } else {
            List.empty[TelluricStar].pure[IO]
          }
        } else {
          IO.raiseError(new RuntimeException("Network error"))
        }
      } else {
        if (shouldSucceed) {
          if (shouldReturnTarget) {
            // Return a mock telluric star
            val mockStar = TelluricStar(
              hip = 12345,
              spType = input.spType.toInput,
              coordinates = input.coordinates,
              distance = 100.0,
              hmag = 8.0,
              score = 0.95,
              order = TelluricCalibrationOrder.Before
            )
            List(mockStar).pure[IO]
          } else {
            List.empty[TelluricStar].pure[IO]
          }
        } else {
          IO.raiseError(new RuntimeException("Network error"))
        }
      }
    }
  }

  // Helper methods for test data
  def createTestProgram: IO[Program.Id] =
    createProgramAs(serviceUser, "Telluric Test Program")

  def createTestObservation(pid: Program.Id): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(serviceUser, pid, Nil)

  def createTestTarget(pid: Program.Id): IO[Target.Id] =
    createTargetAs(serviceUser, pid, "Test Target")

  def randomTime: Timestamp =
    Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().minusDays(1))

  def createPendingEntry(pid: Program.Id, oid: Observation.Id, sid: Observation.Id): TelluricResolution.Pending =
    TelluricResolution.Pending(
      observationId = oid,
      programId = pid,
      scienceObservationId = sid,
      lastInvalidation = randomTime,
      failureCount = 0
    )

  def createMetaEntry(pid: Program.Id, oid: Observation.Id, sid: Observation.Id, state: CalculationState): TelluricResolution.Meta =
    TelluricResolution.Meta(
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

  // Database cleanup method
  def cleanupTelluricResolution: IO[Unit] =
    withSession: session =>
      val cleanup: Command[Void] = sql"DELETE FROM t_telluric_resolution".command
      session.execute(cleanup).void

  // Database helper methods
  def insertPending(pending: TelluricResolution.Pending): IO[Unit] =
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

  def insertMeta(meta: TelluricResolution.Meta): IO[Unit] =
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

  def loadPending: IO[List[TelluricResolution.Pending]] =
    withSession: session =>
      val select: Query[Void, TelluricResolution.Pending] = sql"""
        SELECT c_observation_id,
               c_program_id,
               c_science_observation_id,
               c_last_invalidation,
               c_failure_count
        FROM t_telluric_resolution
        WHERE c_state IN ('pending', 'retry')
        ORDER BY c_last_invalidation
      """.query(observation_id *: program_id *: observation_id *: timestamp *: int4)
        .map { case (oid, pid, scienceOid, lastInv, failCount) =>
          TelluricResolution.Pending(oid, pid, scienceOid,
            Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv), failCount)
        }

      session.prepareR(select).use(_.stream(Void, 1024).compile.toList)

  def loadMeta: IO[List[TelluricResolution.Meta]] =
    withSession: session =>
      val select: Query[Void, TelluricResolution.Meta] = sql"""
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
        TelluricResolution.Meta(oid, pid, scienceOid, state,
          Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv),
          Timestamp.fromLocalDateTimeTruncatedAndBounded(lastUpd),
          retryAt.map(t => Timestamp.fromLocalDateTimeTruncatedAndBounded(t)),
          failCount, targetId, errorMsg)
      }

      session.prepareR(select).use(_.stream(Void, 1024).compile.toList)

  def selectMeta(oid: Observation.Id): IO[Option[TelluricResolution.Meta]] =
    withSession: session =>
      val select: Query[Observation.Id, TelluricResolution.Meta] = sql"""
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
        TelluricResolution.Meta(oid, pid, scienceOid, state,
          Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv),
          Timestamp.fromLocalDateTimeTruncatedAndBounded(lastUpd),
          retryAt.map(t => Timestamp.fromLocalDateTimeTruncatedAndBounded(t)),
          failCount, targetId, errorMsg)
      }

      session.prepareR(select).use(_.option(oid))

  test("database - verify calculation_state codec available"):
    // Verify that calculation_state codec from obscalc is available
    val codec = calculation_state
    assertNotEquals(codec, null, "calculation_state codec should be available")

  test("database - basic compilation test"):
    // For Phase 1, we just verify that our domain types compile
    // and basic database structure would work
    val pending = TelluricResolution.Pending(
      observationId = Observation.Id(PosLong.unsafeFrom(1L)),
      programId = Program.Id(PosLong.unsafeFrom(1L)),
      scienceObservationId = Observation.Id(PosLong.unsafeFrom(2L)),
      lastInvalidation = Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now()),
      failureCount = 0
    )

    val meta = TelluricResolution.Meta(
      observationId = Observation.Id(PosLong.unsafeFrom(1L)),
      programId = Program.Id(PosLong.unsafeFrom(1L)),
      scienceObservationId = Observation.Id(PosLong.unsafeFrom(2L)),
      state = CalculationState.Pending,
      lastInvalidation = Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now()),
      lastUpdate = Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now()),
      retryAt = None,
      failureCount = 0,
      resolvedTargetId = None,
      errorMessage = None
    )

    assertEquals(pending.observationId, Observation.Id(PosLong.unsafeFrom(1L)))
    assertEquals(meta.state, CalculationState.Pending)
    assertEquals(meta.failureCount, 0)

  test("database - insert and retrieve pending entries"):
    for {
      _ <- cleanupTelluricResolution
      pid <- createTestProgram
      oid1 <- createTestObservation(pid)
      oid2 <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert test data
      pending1 = createPendingEntry(pid, oid1, sid)
      pending2 = createPendingEntry(pid, oid2, sid)

      _ <- insertPending(pending1)
      _ <- insertPending(pending2)

      // Load and verify
      loaded <- loadPending

    } yield {
      assertEquals(loaded.length, 2, "Should have 2 pending entries")
      assert(loaded.map(_.observationId).contains(oid1), "Should contain oid1")
      assert(loaded.map(_.observationId).contains(oid2), "Should contain oid2")
    }

  test("database - insert and retrieve meta entries"):
    for {
      _ <- cleanupTelluricResolution
      pid <- createTestProgram
      oid1 <- createTestObservation(pid)
      oid2 <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert test data
      meta1 = createMetaEntry(pid, oid1, sid, CalculationState.Ready)
      meta2 = createMetaEntry(pid, oid2, sid, CalculationState.Retry).copy(
        retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().plusHours(1)))
      )

      _ <- insertMeta(meta1)
      _ <- insertMeta(meta2)

      // Load and verify
      loaded <- loadMeta

    } yield {
      assertEquals(loaded.length, 2, "Should have 2 meta entries")
      assert(loaded.map(_.observationId).contains(oid1), "Should contain oid1")
      assert(loaded.map(_.observationId).contains(oid2), "Should contain oid2")
      assertEquals(loaded.find(_.observationId == oid1).get.state, CalculationState.Ready)
      assertEquals(loaded.find(_.observationId == oid2).get.state, CalculationState.Retry)
    }

  test("database - select specific meta entry"):
    for {
      _ <- cleanupTelluricResolution
      pid <- createTestProgram
      oid <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert test data
      meta = createMetaEntry(pid, oid, sid, CalculationState.Ready)
      _ <- insertMeta(meta)

      // Select specific entry
      selected <- selectMeta(oid)

    } yield {
      assert(selected.isDefined, "Should find meta entry")
      assertEquals(selected.get.observationId, oid, "Should have correct observation ID")
      assertEquals(selected.get.state, CalculationState.Ready, "Should have correct state")
    }

  test("database - empty queries"):
    for {
      _ <- cleanupTelluricResolution
      // Load from empty database
      pending <- loadPending
      meta <- loadMeta

      // Select non-existent entry
      selected <- selectMeta(Observation.Id(PosLong.unsafeFrom(99999L)))

    } yield {
      assertEquals(pending.length, 0, "Should have no pending entries")
      assertEquals(meta.length, 0, "Should have no meta entries")
      assert(selected.isEmpty, "Should not find non-existent entry")
    }

  // TODO: Add comprehensive service tests once basic database integration is verified
  // Service tests require proper Services.forUser setup and transaction handling

}
