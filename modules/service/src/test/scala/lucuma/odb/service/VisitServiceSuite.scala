// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.ACursorOps
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.json.all.transport.given

class VisitServiceSuite extends ExecutionTestSupportForGmos:

  def queryWorkflowState(user: User, oid: Observation.Id): IO[ObservationWorkflowState] =
    query(
      user,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow { value { state } }
          }
        }
      """
    ).map: json =>
      json.hcursor.downFields("observation", "workflow", "value", "state").require[ObservationWorkflowState]

  test("hasVisits"):
    for
      pid    <- createProgramAs(serviceUser)
      oid    <- createObservationAs(serviceUser, pid, ObservingModeType.GmosNorthLongSlit.some)
      before <- withServices(serviceUser): services =>
        services.transactionally:
          services.visitService.hasVisits(oid)
      _      <- recordVisitAs(serviceUser, Instrument.GmosNorth, oid)
      after  <- withServices(serviceUser): services =>
        services.transactionally:
          services.visitService.hasVisits(oid)
    yield
      assertEquals(before, false)
      assertEquals(after, true)

  test("recording a visit, atom, or step changes workflow state to Ongoing"):
    for
      pid              <- createProgram
      tid              <- createTargetWithProfileAs(pi, pid)
      oid              <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _                <- runObscalcUpdate(pid, oid)
      stateBefore      <- queryWorkflowState(pi, oid)
      vid              <- recordVisitAs(serviceUser, Instrument.GmosNorth, oid)
      aid              <- recordAtomAs(serviceUser, Instrument.GmosNorth, vid, SequenceType.Science)
      sid              <- recordStepAs(serviceUser, aid, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
      _                <- addEndStepEvent(sid)
      _                <- runObscalcUpdate(pid, oid)
      stateAfterExec   <- queryWorkflowState(pi, oid)
    yield
      assertEquals(stateBefore, ObservationWorkflowState.Defined)
      assertEquals(stateAfterExec, ObservationWorkflowState.Ongoing)
