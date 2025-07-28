// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given

class executionState extends ExecutionTestSupportForGmos {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(2)
    )

  def stateQuery(o: Observation.Id): String =
    s"""
       query {
         observation(observationId: "$o") {
           execution {
             executionState
           }
         }
       }
     """

  def stateResult(s: ExecutionState): Json =
    json"""
      {
        "observation": {
          "execution": {
            "executionState": ${s.tag.toScreamingSnakeCase}
          }
        }
      }
    """

  import ExecutionState.*

  test("isComplete - NOT_STARTED") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = stateQuery(oid),
        expected = stateResult(NotStarted).asRight
      )
  }

  test("isComplete - ONGOING") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = stateQuery(oid),
        expected = stateResult(Ongoing).asRight
      )
  }

  test("isComplete - COMPLETED") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = stateQuery(oid),
        expected = stateResult(Completed).asRight
      )
  }

  test("isComplete - NOT_DEFINED") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createIncompleteTargetAs(pi, p) // missing data radial velocity & source profile
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = stateQuery(oid),
        expected = stateResult(NotDefined).asRight
      )
  }
}
