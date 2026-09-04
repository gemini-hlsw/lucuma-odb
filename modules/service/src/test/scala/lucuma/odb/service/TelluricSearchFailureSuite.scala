// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.telluric.TelluricSearchInput
import lucuma.catalog.telluric.TelluricStar
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.syntax.timespan.*
import lucuma.core.util.CalculationState
import lucuma.odb.graphql.TestUsers

// A telluric search backend that throws must not kill the daemon: the failure
// is recorded on the row and the resolution is scheduled for retry.
class TelluricSearchFailureSuite extends TelluricTargetsServiceSuiteSupport:

  override val pi = TestUsers.Standard.pi(1, 30)
  override val validUsers = List(pi)

  override protected def telluricClient: IO[TelluricTargetsClient[IO]] =
    IO.pure:
      new TelluricTargetsClient[IO]:
        def search(input: TelluricSearchInput): IO[List[TelluricStar]] =
          IO.raiseError(new RuntimeException("telluric backend is down"))
        def searchTarget(input: TelluricSearchInput): IO[List[(TelluricStar, Option[CatalogTargetResult])]] =
          IO.raiseError(new RuntimeException("telluric backend is down"))

  test("a throwing search backend records the failure and schedules a retry"):
    for
      _     <- cleanup
      pid   <- createProgramAs(pi, "Telluric Failure Program")
      tid   <- createTargetWithProfileAs(pi, pid)
      sid   <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      oid1  <- createTelluricCalibrationObservation(pi, pid)
      oid2  <- createTelluricCalibrationObservation(pi, pid)
      _     <- insertPending(createPendingEntry(pid, oid1, sid, 30.minTimeSpan))
      _     <- insertPending(createPendingEntry(pid, oid2, sid, 30.minTimeSpan))
      // Drains the whole queue; a thrown exception here would fail the test.
      _     <- resolveTelluricTargets
      meta1 <- selectMeta(oid1).map(_.get)
      meta2 <- selectMeta(oid2).map(_.get)
    yield
      List(meta1, meta2).foreach: meta =>
        assertEquals(meta.state, CalculationState.Retry)
        assertEquals(meta.failureCount, 1)
        assert(meta.retryAt.isDefined, "a retry time must be scheduled")
        assert(meta.errorMessage.exists(_.contains("telluric backend is down")), s"unexpected error: ${meta.errorMessage}")
        assertEquals(meta.resolvedTargetId, None)
