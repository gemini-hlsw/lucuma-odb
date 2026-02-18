// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.json.all.transport.given

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
    query(
      pi,
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

  /** Test that we can change to this specified state, and then back. */
  def testTransition(pid: Program.Id, oid: Observation.Id, state: ObservationWorkflowState): IO[Unit] =
    queryObservationWorkflowState(oid).flatMap: current =>
      setObservationWorkflowState(pi, oid, state) >>
      runObscalcUpdate(pid, oid) >>
      assertIO(queryObservationWorkflowState(oid), state) >>
      setObservationWorkflowState(pi, oid, current) >>
      runObscalcUpdate(pid, oid) >>
      assertIO(queryObservationWorkflowState(oid), current)

  /** Test that we can change to the specified states and back, and CANNOT change to anything else. */
  def testTransitions(pid: Program.Id, oid: Observation.Id, allowedTransitions: ObservationWorkflowState*): IO[Unit] =
    val legal = allowedTransitions.toList
    val illegal = ObservationWorkflowState.values.toList.filterNot(legal.contains)
    legal.traverse_(testTransition(pid, oid, _)) >>
    illegal.traverse_ : state =>
      interceptOdbError(testTransition(pid, oid, state)):
        case OdbError.InvalidWorkflowTransition(_, `state`, _) => () // ok

  //
  // TESTS START HERE
  ///

  import ObservationWorkflowState.*

  test("              Undefined  <-> Inactive"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Undefined)
      _   <- testTransitions(pid, oid, Undefined, Inactive)
    } yield ()

  testWithTargetTypes("Unapproved <-> Inactive") { (_, mkTarget) =>
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
  }

  testWithTargetTypes("Defined    <-> Inactive        (proposal not yet accepted)"): (_, mkTarget) =>
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


  test("[Sidereal]    Defined    <-> Inactive, Ready (proposal accepted)"):
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

  test("[Opportunity] Defined    <-> Inactive        (proposal accepted)"):
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
  test("[Sidereal]    Ongoing    <-> Inactive, Completed"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s0, v)
      s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s1, v)
      s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s2, v)
      _  <- runObscalcUpdate(p, o)
      _  <- assertIO(queryObservationWorkflowState(o), Ongoing)
      _  <- testTransitions(p, o, Ongoing, Inactive, Completed)
    yield ()

// (see executionState.scala)
  test("[Sidereal]    Completed  <-> <none> if naturally complete"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

      a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s0, v)
      s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s1, v)
      s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s2, v)

      a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, telescopeConfig(0, 15, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s3, v)
      s4 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthFlat(5), FlatStep, telescopeConfig(0, 15, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s4, v)
      s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthScience(5), StepConfig.Science, telescopeConfig(0, 15, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s5, v)

      _  <- runObscalcUpdate(p, o)
      _  <- assertIO(queryObservationWorkflowState(o), Completed)
      _  <- testTransitions(p, o, Completed)

    yield ()

  test("[Sidereal]    Completed  <-> Ongoing, if explicitly declared complete"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s0, v)
      s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s1, v)
      s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s2, v)
      _  <- runObscalcUpdate(p, o)
      _  <- assertIO(queryObservationWorkflowState(o), Ongoing)
      _  <- setObservationWorkflowState(pi, o, Completed)
      _  <- runObscalcUpdate(p, o)
      _  <- assertIO(queryObservationWorkflowState(o), Completed)
      _  <- testTransitions(p, o, Completed, Ongoing)
    yield ()

  test("[Eng]         Defined    <-> Inactive, Ready"):
    for {
      pid <- createProgramAs(pi)
        _ <- setProgramReference(staff, pid, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
      oid <- createObservationAs(pi, pid)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Defined)
      _   <- testTransitions(pid, oid, Defined, Inactive, Ready)
    } yield ()

  test("[Eng]         Ongoing    <-> Inactive, Completed"):
    for
      p <- createProgram
      _ <- setProgramReference(staff, p, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s0, v)
      s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s1, v)
      s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s2, v)
      _  <- runObscalcUpdate(p, o)
      _  <- assertIO(queryObservationWorkflowState(o), Ongoing)
      _  <- testTransitions(p, o, Ongoing, Inactive, Completed)
    yield ()

  test("[Eng]         Completed  <->"):
    for
      p <- createProgram
      _ <- setProgramReference(staff, p, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s0, v)
      s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s1, v)
      s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s2, v)
      a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, telescopeConfig(0, 15, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s3, v)
      s4 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthFlat(5), FlatStep, telescopeConfig(0, 15, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s4, v)
      s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthScience(5), StepConfig.Science, telescopeConfig(0, 15, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s5, v)
      _  <- runObscalcUpdate(p, o)
      _  <- assertIO(queryObservationWorkflowState(o), Completed)
      _  <- testTransitions(p, o, Completed)
    yield ()

}
