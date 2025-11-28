// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.model.Observation
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.TelluricResolution
import lucuma.odb.graphql.TestUsers

import java.time.LocalDateTime

class TelluricResolutionServiceSuite extends TelluricResolutionServiceSuiteSupport {

  override val serviceUser = TestUsers.service(nextId)
  override val validUsers = List(serviceUser)

  test("service - load pending entries"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid1 <- createTestObservation(pid)
      oid2 <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert test data using direct DB
      pending1 = createPendingEntry(pid, oid1, sid)
      pending2 = createPendingEntry(pid, oid2, sid)

      _ <- insertPending(pending1)
      _ <- insertPending(pending2)

      // Load using service method
      loaded <- load(10)

    } yield {
      assertEquals(loaded.length, 2, "Should have 2 pending entries")
      assert(loaded.map(_.observationId).contains(oid1), "Should contain oid1")
      assert(loaded.map(_.observationId).contains(oid2), "Should contain oid2")
    }

  test("database - insert and retrieve meta entries"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid1 <- createTestObservation(pid)
      oid2 <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert test data using direct DB
      meta1 = createMetaEntry(pid, oid1, sid, CalculationState.Ready)
      meta2 = createMetaEntry(pid, oid2, sid, CalculationState.Retry).copy(
        retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().plusHours(1)))
      )

      _ <- insertMeta(meta1)
      _ <- insertMeta(meta2)

      // Verify using direct DB query
      loaded <- selectAllMeta

    } yield {
      assertEquals(loaded.length, 2, "Should have 2 meta entries")
      assert(loaded.map(_.observationId).contains(oid1), "Should contain oid1")
      assert(loaded.map(_.observationId).contains(oid2), "Should contain oid2")
      assertEquals(loaded.find(_.observationId == oid1).get.state, CalculationState.Ready)
      assertEquals(loaded.find(_.observationId == oid2).get.state, CalculationState.Retry)
    }

  test("database - select specific meta entry"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert test data using direct DB
      meta = createMetaEntry(pid, oid, sid, CalculationState.Ready)
      _ <- insertMeta(meta)

      // Select specific entry using direct DB query
      selected <- selectMeta(oid)

    } yield {
      assert(selected.isDefined, "Should find meta entry")
      assertEquals(selected.get.observationId, oid, "Should have correct observation ID")
      assertEquals(selected.get.state, CalculationState.Ready, "Should have correct state")
    }

  test("service - empty load returns no entries"):
    for {
      _ <- cleanup
      // Load from empty database using service method
      pending <- load(10)

      // Select non-existent entry using direct DB
      selected <- selectMeta(Observation.Id(PosLong.unsafeFrom(99999L)))

    } yield {
      assertEquals(pending.length, 0, "Should have no pending entries")
      assert(selected.isEmpty, "Should not find non-existent entry")
    }

  test("reset - marks calculating entries as pending"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert entry with 'calculating' state
      meta = createMetaEntry(pid, oid, sid, CalculationState.Calculating)
      _ <- insertMeta(meta)

      // Call reset
      _ <- reset

      // Verify state changed to pending
      state <- calculationState(oid)
    } yield assertEquals(state, CalculationState.Pending)

  test("loadObs - loads specific pending observation"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid1 <- createTestObservation(pid)
      oid2 <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert pending entries for both observations
      pending1 = createPendingEntry(pid, oid1, sid)
      pending2 = createPendingEntry(pid, oid2, sid)
      _ <- insertPending(pending1)
      _ <- insertPending(pending2)

      // Load specific observation
      loaded <- loadObs(oid1)

      // Verify state changed to calculating for oid1 only
      state1 <- calculationState(oid1)
      state2 <- calculationState(oid2)
    } yield {
      assert(loaded.isDefined, "Should load pending entry")
      assertEquals(loaded.get.observationId, oid1, "Should load correct observation")
      assertEquals(state1, CalculationState.Calculating, "oid1 should be calculating")
      assertEquals(state2, CalculationState.Pending, "oid2 should remain pending")
    }

  test("load - respects max limit parameter"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      sid <- createTestObservation(pid)

      // Insert 10 pending entries
      oids <- (1 to 10).toList.traverse(_ => createTestObservation(pid))
      _ <- oids.traverse(oid => insertPending(createPendingEntry(pid, oid, sid)))

      // Load only 5
      loaded <- load(5)

      // Verify states
      states <- selectAllMeta
    } yield {
      assertEquals(loaded.length, 5, "Should return exactly 5 entries")
      val calculatingCount = states.count(_.state == CalculationState.Calculating)
      assertEquals(calculatingCount, 5, "Should have exactly 5 calculating entries")
      val pendingCount = states.count(_.state == CalculationState.Pending)
      assertEquals(pendingCount, 5, "Should have 5 pending entries remaining")
    }

  test("load - skips retry entries that aren't ready yet"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      sid <- createTestObservation(pid)
      oid1 <- createTestObservation(pid)
      oid2 <- createTestObservation(pid)
      oid3 <- createTestObservation(pid)

      // Insert 2 pending entries (ready to process)
      _ <- insertPending(createPendingEntry(pid, oid1, sid))
      _ <- insertPending(createPendingEntry(pid, oid2, sid))

      // Insert 1 retry entry with retry_at far in the future
      futureRetry = createMetaEntry(pid, oid3, sid, CalculationState.Retry).copy(
        retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().plusDays(7))),
        failureCount = 2
      )
      _ <- insertMeta(futureRetry)

      // Verify all entries before loading
      allMeta <- selectAllMeta

      // Load all available
      loaded <- load(10)

    } yield {
      // Debug: check what's in the database
      assertEquals(allMeta.length, 3, "Should have 3 entries total")
      val pending = allMeta.filter(_.state == CalculationState.Pending)
      val retry = allMeta.filter(_.state == CalculationState.Retry)
      assertEquals(pending.length, 2, s"Should have 2 pending entries, got: $pending")
      assertEquals(retry.length, 1, s"Should have 1 retry entry, got: $retry")

      val oid3Meta = allMeta.find(_.observationId == oid3)
      assert(oid3Meta.isDefined, "oid3 should exist")
      assertEquals(oid3Meta.get.state, CalculationState.Retry, "oid3 should be in retry state")
      assert(oid3Meta.get.retryAt.isDefined, "oid3 should have retry_at set")

      assertEquals(loaded.length, 2, "Should only return 2 pending entries, skip future retry")
      assert(!loaded.map(_.observationId).contains(oid3), "Should not include future retry entry")
    }

  test("load - includes retry entries past retry_at time"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      sid <- createTestObservation(pid)
      oid <- createTestObservation(pid)

      // Insert retry entry with retry_at in the past
      pastRetry = createMetaEntry(pid, oid, sid, CalculationState.Retry).copy(
        retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().minusHours(1))),
        failureCount = 2
      )
      _ <- insertMeta(pastRetry)

      // Load
      loaded <- load(10)

      // Verify state changed to calculating
      state <- calculationState(oid)
    } yield {
      assertEquals(loaded.length, 1, "Should return retry entry that's past retry time")
      assertEquals(loaded.head.observationId, oid, "Should be the retry entry")
      assertEquals(loaded.head.failureCount, 2, "Should preserve failure count")
      assertEquals(state, CalculationState.Calculating, "State should change to calculating")
    }

  test("invalidateAll - resets all entries to pending"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      sid <- createTestObservation(pid)

      // Insert entries with various states
      oid1 <- createTestObservation(pid)
      oid2 <- createTestObservation(pid)
      oid3 <- createTestObservation(pid)
      oid4 <- createTestObservation(pid)

      _ <- insertMeta(createMetaEntry(pid, oid1, sid, CalculationState.Ready))
      _ <- insertMeta(createMetaEntry(pid, oid2, sid, CalculationState.Retry).copy(
        retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().plusHours(1))),
        failureCount = 3
      ))
      _ <- insertMeta(createMetaEntry(pid, oid3, sid, CalculationState.Calculating))
      _ <- insertMeta(createMetaEntry(pid, oid4, sid, CalculationState.Pending))

      // Invalidate all
      count <- invalidateAll

      // Verify all are pending
      states <- selectAllMeta
    } yield {
      assertEquals(count, 4, "Should invalidate all 4 entries")
      assertEquals(states.length, 4, "Should have 4 entries")
      assert(states.forall(_.state == CalculationState.Pending), "All should be pending")
      assert(states.forall(_.retryAt.isEmpty), "All retry_at should be None")
      assert(states.forall(_.failureCount == 0), "All failure_count should be 0")
    }

  test("load - changes state from pending to calculating"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert pending entry
      _ <- insertPending(createPendingEntry(pid, oid, sid))

      // Verify initial state
      stateBefore <- calculationState(oid)

      // Load
      loaded <- load(1)

      // Verify final state
      stateAfter <- calculationState(oid)
    } yield {
      assertEquals(stateBefore, CalculationState.Pending, "Should start as pending")
      assertEquals(loaded.length, 1, "Should return entry")
      assertEquals(stateAfter, CalculationState.Calculating, "Should change to calculating")
    }

  test("load - changes state from retry to calculating"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert retry entry with retry_at in the past
      retry = createMetaEntry(pid, oid, sid, CalculationState.Retry).copy(
        retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().minusHours(1))),
        failureCount = 2
      )
      _ <- insertMeta(retry)

      // Verify initial state
      stateBefore <- calculationState(oid)

      // Load
      loaded <- load(1)

      // Verify final state
      stateAfter <- calculationState(oid)
    } yield {
      assertEquals(stateBefore, CalculationState.Retry, "Should start as retry")
      assertEquals(loaded.length, 1, "Should return entry")
      assertEquals(stateAfter, CalculationState.Calculating, "Should change to calculating")
    }

  test("load - uses FOR UPDATE SKIP LOCKED correctly"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      sid <- createTestObservation(pid)

      // Insert 5 pending entries
      oids <- (1 to 5).toList.traverse(_ => createTestObservation(pid))
      _ <- oids.traverse(oid => insertPending(createPendingEntry(pid, oid, sid)))

      // First load gets 3
      firstLoad <- load(3)

      // Second load should get remaining 2
      secondLoad <- load(5)

      // Verify states
      states <- selectAllMeta
    } yield {
      assertEquals(firstLoad.length, 3, "First load should get 3 entries")
      assertEquals(secondLoad.length, 2, "Second load should get remaining 2 entries")

      val firstOids = firstLoad.map(_.observationId).toSet
      val secondOids = secondLoad.map(_.observationId).toSet
      assert(firstOids.intersect(secondOids).isEmpty, "No overlap between loads")

      assertEquals(states.count(_.state == CalculationState.Calculating), 5, "All 5 should be calculating")
    }

  test("loadObs - returns None for non-existent observation"):
    for {
      _ <- cleanup
      nonExistent = Observation.Id(PosLong.unsafeFrom(99999L))

      // Try to load non-existent observation
      loaded <- loadObs(nonExistent)
    } yield {
      assert(loaded.isEmpty, "Should return None for non-existent observation")
    }

  test("loadObs - returns None if observation not in pending/retry state"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid <- createTestObservation(pid)
      sid <- createTestObservation(pid)

      // Insert entry with 'ready' state
      _ <- insertMeta(createMetaEntry(pid, oid, sid, CalculationState.Ready))

      // Try to load
      loaded <- loadObs(oid)
    } yield {
      assert(loaded.isEmpty, "Should return None for observation in ready state")
    }

  test("load - handles empty table gracefully"):
    for {
      _ <- cleanup

      // Load from empty table
      loaded <- load(10)
    } yield {
      assertEquals(loaded.length, 0, "Should return empty list for empty table")
    }

  test("selectF2Config - returns telluric type and duration when observation has duration"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid <- createF2Observation(pid)

      // Set observation duration to 3600 seconds (1 hour)
      _ <- setObservationDuration(oid, 3600L)

      // Query F2 config
      result <- selectF2Config(oid)
    } yield {
      assert(result.isDefined, "Should return F2 config")
      val (telluricType, durationMicros) = result.get
      assertEquals(telluricType, lucuma.core.model.TelluricType.Hot, "Should have default telluric type")
      assertEquals(durationMicros, 3600L * 1000000L, "Should convert 3600 seconds to 3600000000 microseconds")
    }

  test("selectF2Config - returns None when observation duration is NULL"):
    for {
      _ <- cleanup
      pid <- createTestProgram
      oid <- createF2Observation(pid)

      // Don't set observation duration (it will be NULL)

      // Query F2 config
      result <- selectF2Config(oid)
    } yield {
      assert(result.isEmpty, "Should return None when observation duration is NULL")
    }

  test("selectF2Config - correctly converts various durations to microseconds"):
    for {
      _ <- cleanup
      pid <- createTestProgram

      // Test multiple durations
      oid1 <- createF2Observation(pid)
      oid2 <- createF2Observation(pid)
      oid3 <- createF2Observation(pid)

      // Set different durations
      _ <- setObservationDuration(oid1, 1L)      // 1 second
      _ <- setObservationDuration(oid2, 60L)     // 1 minute
      _ <- setObservationDuration(oid3, 7200L)   // 2 hours

      // Query all configs
      result1 <- selectF2Config(oid1)
      result2 <- selectF2Config(oid2)
      result3 <- selectF2Config(oid3)
    } yield {
      assertEquals(result1.get._2, 1L * 1000000L, "1 second should be 1000000 microseconds")
      assertEquals(result2.get._2, 60L * 1000000L, "60 seconds should be 60000000 microseconds")
      assertEquals(result3.get._2, 7200L * 1000000L, "7200 seconds should be 7200000000 microseconds")
    }

}
