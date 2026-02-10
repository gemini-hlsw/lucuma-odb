// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.Services.Syntax.*

class SequenceServiceSuite extends ExecutionTestSupportForGmos:

  override val FakeItcResult: IntegrationTime =
    IntegrationTime(
      10.secTimeSpan,
      PosInt.unsafeFrom(100)
    )

  private def generateSequence(
    o: Observation.Id
  ): IO[List[Atom[GmosNorth]]] =
    withServices(serviceUser): services =>
      services.generator.generate(o).map: e =>
        val g =
          e.toOption
           .flatMap: iec =>
             InstrumentExecutionConfig.gmosNorth.getOption(iec)
           .get.executionConfig.science.get
        g.nextAtom :: g.possibleFuture

  private def readSequence(
    o: Observation.Id
  ): IO[List[Atom[GmosNorth]]] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          sequenceService
            .selectGmosNorthExecutionConfig(o)
            .map(_.traverse(_.science))
            .flatMap(_.collect { case Some(a) => a }.compile.toList)

  test("exercise serialization"):
    val res = for
      p  <- createProgram
      t  <- createTargetWithProfileAs(pi, p)
      o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      sn <- generateSequence(o)

      b  <- IO.realTimeInstant
      _  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      e0 <- IO.realTimeInstant
      _  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      e1 <- IO.realTimeInstant

      so <- readSequence(o)
      e2 <- IO.realTimeInstant

//      _  <- IO.println(s"WRITE TIME: ${java.time.Duration.between(b,  e0)}")
//      _  <- IO.println(s"NEW VISIT.: ${java.time.Duration.between(e0, e1)}")
//      _  <- IO.println(s"READ TIME.: ${java.time.Duration.between(e1, e2)}")
//      _  <- IO.println(s"SEQUENCE.n: ${sn.mkString("\n")}")
//      _  <- IO.println("----")
//      _  <- IO.println(s"SEQUENCE.o: ${so.mkString("\n")}")
    yield (sn, so)

    assertIOBoolean(res.map((in, out) => in == out))
