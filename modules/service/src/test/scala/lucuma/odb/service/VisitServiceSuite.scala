// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class VisitServiceSuite extends ExecutionTestSupportForGmos:

  test("hasVisits"):
    for {
      pid    <- createProgramAs(serviceUser)
      oid    <- createObservationAs(serviceUser, pid, ObservingModeType.GmosNorthLongSlit.some)
      before <- withServices(serviceUser): services =>
                  services.transactionally:
                    services.visitService.hasVisits(oid)
      _      <- recordVisitAs(serviceUser, Instrument.GmosNorth, oid)
      after  <- withServices(serviceUser): services =>
                  services.transactionally:
                    services.visitService.hasVisits(oid)
    } yield {
      assert(!before)
      assert(after)
    }
