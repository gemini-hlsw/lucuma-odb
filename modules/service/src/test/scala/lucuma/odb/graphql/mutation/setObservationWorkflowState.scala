// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.core.model.User

class setObservationWorkflowState
  extends ExecutionTestSupportForGmos
     with ObservingModeSetupOperations
     with UpdateObservationsOps {


  // required in order to get the correct "complete" execution status below (see executionState.scala)
  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(2)
    )

  def approveConfigurationRequest(req: ConfigurationRequest.Id): IO[Unit] =
    import skunk.syntax.all.*
    import lucuma.odb.util.Codecs.configuration_request_id
    session.use: s =>
      s.prepareR(sql"update t_configuration_request set c_status = 'approved' where c_configuration_request_id = $configuration_request_id".command).use: ps =>
        ps.execute(req).void

  def queryObservationWorkflowState(oid: Observation.Id): IO[ObservationWorkflowState] =
    queryObservationWorkflowStateAs(pi, oid)

  def queryObservationWorkflowStateAs(user: User, oid: Observation.Id): IO[ObservationWorkflowState] =
    query(
      user,
      s"""
        query {
          observation(observationId: "$oid") {
            workflow {
              value {
                state
              }
            }
          }
        }
        """
    ).map: json =>
      json.hcursor.downFields("observation", "workflow", "value", "state").require[ObservationWorkflowState]

  def testTransition(pid: Program.Id, oid: Observation.Id, state: ObservationWorkflowState): IO[Unit] =
    testTransitionAs(pi, pid, oid, state)

  /** Test that we can change to this specified state, and then back. */
  def testTransitionAs(user: User, pid: Program.Id, oid: Observation.Id, state: ObservationWorkflowState): IO[Unit] =
    queryObservationWorkflowState(oid).flatMap: current =>
      setObservationWorkflowState(user, oid, state) >>
      runObscalcUpdate(pid, oid) >>
      assertIO(queryObservationWorkflowStateAs(user, oid), state) >>
      setObservationWorkflowState(user, oid, current) >>
      runObscalcUpdate(pid, oid) >>
      assertIO(queryObservationWorkflowStateAs(user, oid), current)

  def testTransitions(pid: Program.Id, oid: Observation.Id, allowedTransitions: ObservationWorkflowState*): IO[Unit] =
    testTransitionsAs(pi, pid, oid, allowedTransitions*)

  /** Test that we can change to the specified states and back, and CANNOT change to anything else. */
  def testTransitionsAs(user: User, pid: Program.Id, oid: Observation.Id, allowedTransitions: ObservationWorkflowState*): IO[Unit] =
    val legal = allowedTransitions.toList
    val illegal = ObservationWorkflowState.values.toList.filterNot(legal.contains)
    legal.traverse_(testTransitionAs(user, pid, oid, _)) >>
    illegal.traverse_ : state =>
      interceptOdbError(testTransitionAs(user, pid, oid, state)):
        case OdbError.InvalidWorkflowTransition(_, `state`, _) => () // ok

  //
  // TESTS START HERE
  ///

  import ObservationWorkflowState.*

  test("              Undefined  <-> Inactive".ignore):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Undefined)
      _   <- testTransitions(pid, oid, Undefined, Inactive)
    } yield ()

  testWithTargetTypes("Unapproved <-> Inactive".ignore): (_, mkTarget) =>
    for {
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "Foo")
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      tid <- mkTarget(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Unapproved)
      _   <- testTransitions(pid, oid, Unapproved, Inactive)
    } yield ()

  testWithTargetTypes("Defined    <-> Inactive        (proposal not yet accepted)".ignore): (_, mkTarget) =>
    for {
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramAs(pi, "Foo")
      _   <- addProposal(pi, pid, Some(cfp), None)
      tid <- mkTarget(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequest)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Defined)
      _   <- testTransitions(pid, oid, Defined, Inactive)
    } yield ()


  test("[Sidereal]    Defined    <-> Inactive, Ready (proposal accepted)".ignore):
    for {
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "Foo")
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequest)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Defined)
      _   <- testTransitions(pid, oid, Defined, Inactive, Ready)
    } yield ()

  test("[Opportunity] Defined    <-> Inactive        (proposal accepted)".ignore):
    for {
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "Foo")
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      tid <- createOpportunityTargetAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequest)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Defined)
      _   <- testTransitions(pid, oid, Defined, Inactive)
    } yield ()

  // (see executionState.scala)
  test("[Sidereal]    Ongoing    <-> Inactive, Completed".ignore):
    for
      p  <- createProgram
      t  <- createTargetWithProfileAs(pi, p)
      o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v  <- recordVisitAs(serviceUser, o)
      s  <- firstScienceAtomStepIds(serviceUser, o)
      _  <- addEndStepEvent(s(0), v)
      _  <- addEndStepEvent(s(1), v)
      _  <- addEndStepEvent(s(2), v)
      _  <- runObscalcUpdate(p, o)
      _  <- assertIO(queryObservationWorkflowState(o), Ongoing)
      _  <- testTransitions(p, o, Ongoing, Inactive, Completed)
    yield ()

// (see executionState.scala)
  test("[Sidereal]    Completed  <-> <none> if naturally complete".ignore):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, o)
      s <- scienceStepIds(serviceUser, o)
      _ <- s.traverse(sid => addEndStepEvent(sid, v))
      _ <- runObscalcUpdate(p, o)
      _ <- assertIO(queryObservationWorkflowState(o), Completed)
      _ <- testTransitions(p, o, Completed)
    yield ()

  test("[Sidereal]    Completed  <-> Ongoing, if explicitly declared complete".ignore):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, o)
      s <- firstScienceAtomStepIds(serviceUser, o)
      _ <- s.traverse(sid => addEndStepEvent(sid, v))
      _ <- runObscalcUpdate(p, o)
      _ <- assertIO(queryObservationWorkflowState(o), Ongoing)
      _ <- setObservationWorkflowState(pi, o, Completed)
      _ <- runObscalcUpdate(p, o)
      _ <- assertIO(queryObservationWorkflowState(o), Completed)
      _ <- testTransitions(p, o, Completed, Ongoing)
    yield ()

  test("[Eng]         Defined    <-> Inactive, Ready".ignore):
    for {
      pid <- createProgramAs(pi)
        _ <- setProgramReference(staff, pid, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
      oid <- createObservationAs(pi, pid)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Defined)
      _   <- testTransitions(pid, oid, Defined, Inactive, Ready)
    } yield ()

  test("[Eng]         Ongoing    <-> Inactive, Completed".ignore):
    for
      p <- createProgram
      _ <- setProgramReference(staff, p, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, o)
      s <- firstScienceAtomStepIds(serviceUser, o)
      _ <- s.traverse(sid => addEndStepEvent(sid, v))
      _ <- runObscalcUpdate(p, o)
      _ <- assertIO(queryObservationWorkflowState(o), Ongoing)
      _ <- testTransitions(p, o, Ongoing, Inactive, Completed)
    yield ()

  test("[Eng]         Completed  <->".ignore):
    for
      p <- createProgram
      _ <- setProgramReference(staff, p, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, o)
      s <- scienceStepIds(serviceUser, o)
      _ <- s.traverse(sid => addEndStepEvent(sid, v))
      _ <- runObscalcUpdate(p, o)
      _ <- assertIO(queryObservationWorkflowState(o), Completed)
      _ <- testTransitions(p, o, Completed)
    yield ()

  val visitorMode = 
    """
      visitor: {
        mode: MAROON_X
        centralWavelength: {
          picometers: 123
        }
        guideStarMinSep: {
          arcminutes: 1.23
        }
      }
    """

  test("[Visitor]      Defined <-> Inactive, Ready"):
    for
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), visitorMode)
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequest)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _ <- assertIO(queryObservationWorkflowState(oid), Defined)
      _ <- testTransitions(pid, oid, Defined, Inactive, Ready)
    yield ()

  // N.B. you can't go from Inactive straight to ready, you have to go via Defined
  test("[Visitor]      Ready -> Inactive -> Defined -> Ready"):
    for
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), visitorMode)
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequest)
      _   <- setObservationWorkflowState(pi, oid, Ready)
      _   <- setObservationWorkflowState(pi, oid, Inactive)
      _   <- setObservationWorkflowState(pi, oid, Defined)
      _   <- setObservationWorkflowState(pi, oid, Ready)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Ready)
    yield ()

  test("[Visitor]      Ready -> Ongoing (disallowed for PIs)"):
    for
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), visitorMode)
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequest)
      _   <- setObservationWorkflowState(pi, oid, Ready)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Ready)
      _   <- interceptOdbError(setObservationWorkflowState(pi, oid, Ongoing)):
              case OdbError.InvalidWorkflowTransition(Ready, Ongoing, _) => () // expected
    yield ()

  test("[Visitor]      Ready -> Ongoing -> Ready (allowed for Staff)"):
    for
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), visitorMode)
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequest)
      _   <- setObservationWorkflowState(pi, oid, Ready)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Ready)
      _   <- setObservationWorkflowState(staff, oid, Ongoing)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Ongoing)
      _   <- setObservationWorkflowState(staff, oid, Ready)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Ready)
    yield ()

}
