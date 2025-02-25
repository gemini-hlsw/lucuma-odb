// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.data.Ior
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObservationWorkflowState.Completed
import lucuma.core.enums.ObservationWorkflowState.Ongoing
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.mutation.UpdateConstraintSetOps
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.json.all.transport.given

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
      _  <- computeItcResultAs(pi,o)
      _  <- assertIO(queryObservationWorkflowState(o), state)
    yield o

  def tryUpdateSubtitleAs(
    user:     User,
    oids: List[Observation.Id],
    expected: Ior[List[String], Json]
  ) =
    val query =
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              subtitle: "new value"
            }
            WHERE: {
              id: { 
                IN: ${oids.asJson.noSpaces} 
              }
            }
          }) {
            observations {
              id
            }
          }
        }
      """
    expectIor(user, query, expected)

  def tryUpdateAsterismsAs(
    user: User,
    oid: Observation.Id,
    tid: Target.Id,
    expected: Ior[List[String], Json]
  ) = 
    val query =
      s"""
        mutation {
          updateAsterisms(input: {
            SET: {
              ADD: [${tid.asJson}]
            }
            WHERE: {
              id: { 
                EQ: ${oid.asJson} 
              }
            }
          }) {
            observations {
              id
            }
          }
        }
      """
    expectIor(user, query, expected)

  def tryUpdateGroupIndex(
    user: User,
    oids: List[Observation.Id],
    expected: Ior[List[String], Json]
  ) =
    val query =
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              groupIndex: 0
            }
            WHERE: {
              id: { 
                IN: ${oids.asJson.noSpaces} 
              }
            }
          }) {
            observations {
              id
            }
          }
        }
      """
    expectIor(user, query, expected)
    


  test(s"Ongoing observations should not be editable") {

    val setup: IO[(Observation.Id, Observation.Id)] =
      for 
        pid <- createProgramAs(pi)
        o1  <- createExecutedObservation(pid, Ongoing)
        o2  <- createObservationAs(pi, pid)
      yield (o1,o2)
      
    setup.flatMap: (ongoing, undefined) =>
      tryUpdateSubtitleAs(
        user = pi,
        oids = List(ongoing, undefined),
        expected = Ior.Both(
          List(
            s"Observation $ongoing is ineligibile for this operation due to its workflow state (Ongoing with allowed transition to Inactive)."
          ),
          json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "id": $undefined
                }
              ]
            }
          }
          """
        )
      )

  }


  test(s"Ongoing observations should not be editable, even when set to Inactive") {

    val setup: IO[(Observation.Id, Observation.Id)] =
      for 
        pid <- createProgramAs(pi)
        o1  <- createExecutedObservation(pid, Ongoing)
        _   <- setObservationWorkflowState(pi, o1, ObservationWorkflowState.Inactive)
        o2  <- createObservationAs(pi, pid)
      yield (o1,o2)
      
    setup.flatMap: (inactive, undefined) =>
      tryUpdateSubtitleAs(
        user = pi,
        oids = List(inactive, undefined),
        expected = Ior.Both(
          List(
            s"Observation $inactive is ineligibile for this operation due to its workflow state (Inactive with allowed transition to Ongoing)."
          ),
          json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "id": $undefined
                }
              ]
            }
          }
          """
        )
      )

  }


  test(s"Completed observations should not be editable") {

    val setup: IO[(Observation.Id, Observation.Id)] =
      for 
        pid <- createProgramAs(pi)
        o1  <- createExecutedObservation(pid, Completed)
        o2  <- createObservationAs(pi, pid)
      yield (o1,o2)
      
    setup.flatMap: (completed, undefined) =>
      tryUpdateSubtitleAs(
        user = pi,
        oids = List(completed, undefined),
        expected = Ior.Both(
          List(
            s"Observation $completed is ineligibile for this operation due to its workflow state (Completed)."
          ),
          json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "id": $undefined
                }
              ]
            }
          }
          """
        )
      )

  }


  test(s"Ongoing observations should allow updateObservationsTimes") {

    val setup: IO[Observation.Id] =
      for 
        pid <- createProgramAs(pi)
        o   <- createExecutedObservation(pid, Ongoing)
      yield o
      
    setup.flatMap: oid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateObservationsTimes(
              input: {
                SET: {
                  observationDuration: {
                    hours: 1.23
                  }
                }
                WHERE: {
                  id: {
                    EQ: ${oid.asJson}
                  }
                }
              }
            ) {
              observations {
                id
              }
            }
          }
        """,
        expected = Right(json"""
          {
            "updateObservationsTimes" : {
              "observations" : [
                {
                  "id" : $oid
                }
              ]
            }
          }
        """)    
      )

  }
  
  test(s"Ongoing observations should not allow asterism edits") {

    val setup: IO[(Observation.Id, Target.Id)] =
      for 
        pid <- createProgramAs(pi)
        oid <- createExecutedObservation(pid, Ongoing)
        tid <- createTargetAs(pi, pid)
      yield (oid, tid)
    
    setup.flatMap: (oid, tid) =>
      tryUpdateAsterismsAs(pi, oid, tid,
        Ior.Both(
          List(s"Observation $oid is ineligibile for this operation due to its workflow state (Ongoing with allowed transition to Inactive)."),
          json"""
            {
              "updateAsterisms": {
                  "observations": []
              }
            }
          """
        )
      )
 
  }
  
  test(s"Completed observations should not allow asterism edits") {

    val setup: IO[(Observation.Id, Target.Id)] =
      for 
        pid <- createProgramAs(pi)
        oid <- createExecutedObservation(pid, Completed)
        tid <- createTargetAs(pi, pid)
      yield (oid, tid)
    
    setup.flatMap: (oid, tid) =>
      tryUpdateAsterismsAs(pi, oid, tid,
        Ior.Both(
          List(s"Observation $oid is ineligibile for this operation due to its workflow state (Completed)."),
          json"""
            {
              "updateAsterisms": {
                  "observations": []
              }
            }
          """
        )
      )
 
  }
  test(s"Ongoing observations should not allow their asterism's targets to be edited".ignore):
    ()

  List(Ongoing, Completed).foreach { state =>

    test(s"$state observations *should* be movable") {
      val setup: IO[(Observation.Id, Observation.Id)] =
        for 
          pid <- createProgramAs(pi)
          o1  <- createExecutedObservation(pid, state)
          o2  <- createObservationAs(pi, pid)
        yield (o1,o2)
        
      setup.flatMap: (ongoing, undefined) =>
        tryUpdateGroupIndex(
          user = pi,
          oids = List(ongoing, undefined),
          expected = Ior.Right(
            json"""
            {
              "updateObservations": {
                "observations": [
                  {
                    "id": $ongoing
                  },
                  {
                    "id": $undefined
                  }
                ]
              }
            }
            """
          )
        )

    }
  }
  
  test("Ongoing observations should not allow guide star changes (PI)".ignore):
    ()


  test("Ongoing observations *should* allow guide star changes (Staff)".ignore):
    ()


  test("Ongoing observations should not allow position angle changes (PI)".ignore):
    ()


  test("Ongoing observations *should* allow position angle changes (Staff)".ignore):
    ()


  test("Ongoing observations should not allow acquisition time changes (PI)".ignore):
    ()


  test("Ongoing observations *should* allow acquisition time changes (Staff)".ignore):
    ()


    
}