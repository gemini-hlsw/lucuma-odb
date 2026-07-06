// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.TestUsers

import java.time.LocalDateTime

class CalibrationCalcServiceSuite extends CalibrationCalcServiceSuiteSupport {

  override val pi = TestUsers.Standard.pi(1, 30)
  override val validUsers = List(pi)

  test("loadObs loads a specific pending row and moves it to calculating"):
    for
      _      <- cleanup
      pid    <- createProgramAs(pi, "Calib Calc Test")
      oid1   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      oid2   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      now    <- timestampNow
      _      <- insertPending(pid, oid1, now)
      _      <- insertPending(pid, oid2, now)
      loaded <- loadObs(oid1)
      s1     <- calculationState(oid1)
      s2     <- calculationState(oid2)
    yield
      assert(loaded.isDefined)
      assertEquals(loaded.get.observationId, oid1)
      assertEquals(s1, CalculationState.Calculating)
      assertEquals(s2, CalculationState.Pending)

  test("load respects the limit"):
    for
      _      <- cleanup
      pid    <- createProgramAs(pi, "Calib Calc Test")
      oids   <- (1 to 10).toList.traverse(_ => createFlamingos2LongSlitObservationAs(pi, pid, Nil))
      now    <- timestampNow
      _      <- oids.traverse(oid => insertPending(pid, oid, now))
      loaded <- load(5)
      states <- oids.traverse(calculationState)
    yield
      assertEquals(loaded.length, 5)
      assertEquals(states.count(_ === CalculationState.Calculating), 5)
      assertEquals(states.count(_ === CalculationState.Pending), 5)

  test("load honors c_retry_at: skips future, includes past"):
    for
      _         <- cleanup
      pid       <- createProgramAs(pi, "Calib Calc Test")
      futureOid <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      pastOid   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      now       <- timestampNow
      _         <- insertState(
                     pid, futureOid, CalculationState.Retry, now,
                     retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().plusDays(7))),
                     failureCount = 2
                   )
      _         <- insertState(
                     pid, pastOid, CalculationState.Retry, now,
                     retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().minusHours(1))),
                     failureCount = 2
                   )
      loaded    <- load(10)
      sFuture   <- calculationState(futureOid)
      sPast     <- calculationState(pastOid)
    yield
      assertEquals(loaded.map(_.observationId), List(pastOid))
      assertEquals(sFuture, CalculationState.Retry)
      assertEquals(sPast, CalculationState.Calculating)

  test("reset moves calculating rows back to pending (or retry if c_retry_at is set)"):
    for
      _         <- cleanup
      pid       <- createProgramAs(pi, "Calib Calc Test")
      oid       <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      retryOid  <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      now       <- timestampNow
      _         <- insertState(pid, oid, CalculationState.Calculating, now)
      _         <- insertState(
                     pid, retryOid, CalculationState.Calculating, now,
                     retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().plusHours(1))),
                     failureCount = 1
                   )
      _         <- reset
      s1        <- calculationState(oid)
      s2        <- calculationState(retryOid)
    yield
      assertEquals(s1, CalculationState.Pending)
      assertEquals(s2, CalculationState.Retry)

  test("markReady moves to ready, or back to pending if invalidated mid-flight"):
    for
      _       <- cleanup
      pid     <- createProgramAs(pi, "Calib Calc Test")
      okOid   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      raceOid <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      now     <- timestampNow
      _       <- insertPending(pid, okOid, now)
      _       <- insertPending(pid, raceOid, now)
      okPend  <- loadObs(okOid)
      rcPend  <- loadObs(raceOid)
      // A newer invalidation arrives while raceOid is calculating.
      _       <- insertPending(pid, raceOid, Timestamp.unsafeFromInstantTruncated(now.toInstant.plusSeconds(60)))
      _       <- okPend.traverse_(markReady)
      _       <- rcPend.traverse_(markReady)
      sOk     <- calculationState(okOid)
      sRace   <- calculationState(raceOid)
    yield
      assertEquals(sOk, CalculationState.Ready)
      assertEquals(sRace, CalculationState.Pending)

  test("markRetry backs off with a cap and never goes terminal"):
    for
      _     <- cleanup
      pid   <- createProgramAs(pi, "Calib Calc Test")
      oid   <- createFlamingos2LongSlitObservationAs(pi, pid, Nil)
      now   <- timestampNow
      _     <- insertPending(pid, oid, now)
      _     <- loadObs(oid)
      _     <- markRetry(oid, "boom")
      row1  <- selectRow(oid)
      // Failure number 8: still retrying, backoff capped at 2^5 minutes.
      _     <- insertState(
                 pid, oid, CalculationState.Retry, now,
                 retryAt = Some(Timestamp.fromLocalDateTimeTruncatedAndBounded(LocalDateTime.now().minusHours(1))),
                 failureCount = 7
               )
      _     <- loadObs(oid)
      _     <- markRetry(oid, "still failing")
      row8  <- selectRow(oid)
    yield
      val (state1, _, retryAt1, count1, error1) = row1.get
      assertEquals(state1, CalculationState.Retry)
      assert(retryAt1.isDefined, "retry_at should be set")
      assertEquals(count1, 1)
      assertEquals(error1, Some("boom"))
      val (state8, _, retryAt8, count8, error8) = row8.get
      assertEquals(state8, CalculationState.Retry)
      assertEquals(count8, 8)
      assert(retryAt8.exists(_.toInstant.isBefore(java.time.Instant.now.plusSeconds(33 * 60))))
      assertEquals(error8, Some("still failing"))
}
