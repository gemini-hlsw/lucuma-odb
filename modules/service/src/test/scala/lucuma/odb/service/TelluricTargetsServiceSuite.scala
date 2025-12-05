// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.TelluricTargets
import lucuma.odb.graphql.TestUsers

import java.time.LocalDateTime

class TelluricTargetsServiceSuite extends TelluricTargetsServiceSuiteSupport {

  override val pi = TestUsers.Standard.pi(1, 30)
  override val validUsers = List(pi)

  test("loadObs - loads specific pending observation"):
    for {
      _        <- cleanup
      pid      <- createProgramAs(pi, "Telluric Test Program")
      oid1     <- createTelluricCalibrationObservation(pi, pid)
      oid2     <- createTelluricCalibrationObservation(pi, pid)
      sid      <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      // Insert pending entries for both observations
      pending1 = createPendingEntry(pid, oid1, sid)
      pending2 = createPendingEntry(pid, oid2, sid)
      _        <- insertPending(pending1)
      _        <- insertPending(pending2)
      // Load specific observation
      loaded <- loadObs(oid1)
      state1 <- calculationState(oid1)
      state2 <- calculationState(oid2)
    } yield {
      assert(loaded.isDefined)
      assertEquals(loaded.get.observationId, oid1)
      assertEquals(state1, CalculationState.Calculating)
      assertEquals(state2, CalculationState.Pending)
    }

  test("load with limit"):
    for {
      _      <- cleanup
      pid    <- createProgramAs(pi, "Telluric Test Program")
      sid    <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      // Insert 10 pending entries
      oids   <- (1 to 10).toList.traverse(_ => createTelluricCalibrationObservation(pi, pid))
      _      <- oids.traverse(oid => insertPending(createPendingEntry(pid, oid, sid)))
      // Load 5
      loaded <- load(5)
      // Verify states
      states <- selectAllMeta
    } yield {
      assertEquals(loaded.length, 5)
      val calculatingCount = states.count(_.state === CalculationState.Calculating)
      assertEquals(calculatingCount, 5)
      val pendingCount = states.count(_.state === CalculationState.Pending)
      assertEquals(pendingCount, 5)
    }

  test("skips retry entries that aren't ready yet"):
    for {
      _           <- cleanup
      pid         <- createProgramAs(pi, "Telluric Test Program")
      sid         <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      oid1        <- createTelluricCalibrationObservation(pi, pid)
      oid2        <- createTelluricCalibrationObservation(pi, pid)
      oid3        <- createTelluricCalibrationObservation(pi, pid)
      _           <- insertPending(createPendingEntry(pid, oid1, sid))
      _           <- insertPending(createPendingEntry(pid, oid2, sid))
      futureRetry = createMetaEntry(pid, oid3, sid, CalculationState.Retry)
                      .copy(
                        retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().plusDays(7))),
                        failureCount = 2
                      )
      _           <- insertMeta(futureRetry)
      allMeta     <- selectAllMeta
      loaded      <- load(10)
    } yield {
      assertEquals(allMeta.length, 3)
      val pending = allMeta.filter(_.state === CalculationState.Pending)
      val retry = allMeta.filter(_.state === CalculationState.Retry)
      assertEquals(pending.length, 2)
      assertEquals(retry.length, 1)

      val oid3Meta = allMeta.find(_.observationId === oid3)
      assert(oid3Meta.isDefined)
      assertEquals(oid3Meta.get.state, CalculationState.Retry)
      assert(oid3Meta.get.retryAt.isDefined)

      assertEquals(loaded.length, 2)
      assert(!loaded.map(_.observationId).contains(oid3))
    }

  test("includes retry entries past retry_at time"):
    for {
      _         <- cleanup
      pid       <- createProgramAs(pi, "Telluric Test Program")
      sid       <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      oid       <- createTelluricCalibrationObservation(pi, pid)
      // Insert retry entry with retry_at in the past
      pastRetry = createMetaEntry(pid, oid, sid, CalculationState.Retry)
                    .copy(
                      retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().minusHours(1))),
                      failureCount = 2
                    )
      _         <- insertMeta(pastRetry)
      loaded    <- load(10)
      state     <- calculationState(oid)
    } yield {
      assertEquals(loaded.length, 1)
      assertEquals(loaded.head.observationId, oid)
      assertEquals(loaded.head.failureCount, 2)
      assertEquals(state, CalculationState.Calculating)
    }

  test("changes state from pending to calculating"):
    for {
      _      <- cleanup
      pid    <- createProgramAs(pi, "Telluric Test Program")
      oid    <- createTelluricCalibrationObservation(pi, pid)
      sid    <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      _      <- insertPending(createPendingEntry(pid, oid, sid))
      before <- calculationState(oid)
      // Load
      loaded <- load(1)
      after  <- calculationState(oid)
    } yield {
      assertEquals(before, CalculationState.Pending)
      assertEquals(loaded.length, 1)
      assertEquals(after, CalculationState.Calculating)
    }

  test("changes state from retry to calculating"):
    for {
      _     <- cleanup
      pid   <- createProgramAs(pi, "Telluric Test Program")
      oid   <- createTelluricCalibrationObservation(pi, pid)
      sid   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      // Insert retry entry with retry_at in the past
      retry = createMetaEntry(pid, oid, sid, CalculationState.Retry)
                .copy(
                  retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().minusHours(1))),
                  failureCount = 2
                )
      _      <- insertMeta(retry)
      before <- calculationState(oid)
      // Load
      loaded <- load(1)
      after  <- calculationState(oid)
    } yield {
      assertEquals(before, CalculationState.Retry)
      assertEquals(loaded.length, 1)
      assertEquals(after, CalculationState.Calculating)
    }

}
