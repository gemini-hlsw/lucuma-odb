// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.model.{Observation, Program}
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.TelluricResolution
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.util.Codecs.*

import java.time.LocalDateTime

class TelluricResolutionServiceSuite extends OdbSuite {

  val serviceUser = TestUsers.service(nextId)
  lazy val validUsers = List(serviceUser)

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

  test("database - verify calculation_state codec available"):
    // Verify that the calculation_state codec from obscalc is available
    val codec = calculation_state
    assertNotEquals(codec, null, "calculation_state codec should be available")

}