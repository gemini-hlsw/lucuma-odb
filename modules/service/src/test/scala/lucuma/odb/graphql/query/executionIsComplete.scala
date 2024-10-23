// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given
import skunk.implicits.*

class executionIsComplete extends ExecutionTestSupport {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(2),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  def isCompleteQuery(o: Observation.Id): String =
    s"""
       query {
         observation(observationId: "$o") {
           execution {
             isComplete
           }
         }
       }
     """

  def isCompleteResult(b: Boolean): Json =
    json"""
      {
        "observation": {
          "execution": {
            "isComplete": $b
          }
        }
      }
    """

  test("isComplete - 0/2") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = isCompleteQuery(oid),
        expected = isCompleteResult(false).asRight
      )
  }

  test("isComplete - 1/2") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = isCompleteQuery(oid),
        expected = isCompleteResult(false).asRight
      )
  }

  test("isComplete - 2/2") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = isCompleteQuery(oid),
        expected = isCompleteResult(true).asRight
      )
  }

  test("isComplete - missing data") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetAs(pi, p) // missing data radial velocity & source profile
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = isCompleteQuery(oid),
        expected = isCompleteResult(false).asRight
      )
  }
}