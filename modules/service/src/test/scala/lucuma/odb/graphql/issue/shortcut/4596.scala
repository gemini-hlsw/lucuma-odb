// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObservationWorkflowState.{ Ongoing, Completed }
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.json.all.transport.given
import lucuma.core.model.Program
import lucuma.odb.graphql.mutation.UpdateConstraintSetOps
import io.circe.literal.*

//https://app.shortcut.com/lucuma/story/4596/api-should-prevent-editing-of-observations-for-which-execution-has-started
class ShortCut_4596 extends OdbSuite 
  with ExecutionTestSupport 
  with ObservingModeSetupOperations 
  with UpdateConstraintSetOps {

  // required in order to get the correct "complete" execution status below (see executionState.scala)
  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      NonNegInt.unsafeFrom(2),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  // copied from setObservationWorkflowState
  def queryObservationWorkflowState(oid: Observation.Id): IO[ObservationWorkflowState] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            workflow {
              state
            }
          }
        }
        """
    ).map: json =>
      json.hcursor.downFields("observation", "workflow", "state").require[ObservationWorkflowState]

  def createExecutedObservation(p: Program.Id, state: ObservationWorkflowState): IO[Observation.Id] =
    for
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.PartnerCal)
      _  <- addEndStepEvent(s0)
      s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.PartnerCal)
      _  <- addEndStepEvent(s1)
      s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s2)
      s3 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science).flatMap(addEndStepEvent).whenA(state === Completed)
      _  <- computeItcResult(pi,o)
      _  <- assertIO(queryObservationWorkflowState(o), state)
    yield o

  List(Ongoing).foreach: state =>

    test(s"$state observations should not be editable"):
      createProgramAs(pi).flatMap: pid =>
        createExecutedObservation(pid, state).flatMap: oid =>
          updateObservation(
            user = pi,
            oid = oid,
            update = """
              subtitle: "new subtitle"
            """,
            query = """
              observations {
                id
              }
            """,
            expected = Right(json"42")
          )

    test(s"$state observations should allow updateObsevationTimes".ignore):
      ()

    test(s"$state observations should not allow asterism edits".ignore):
      ()

    test(s"$state observations should not allow their asterism's targets to be edited".ignore):
      ()


  }