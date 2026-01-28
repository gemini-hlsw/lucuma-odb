// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.sequence.gmos.InitialConfigs.GmosNorthStatic
import lucuma.odb.service.Services.Syntax.*

class SequenceServiceSuite extends ExecutionTestSupportForGmos:

  override val FakeItcResult: IntegrationTime =
    IntegrationTime(
      10.secTimeSpan,
      PosInt.unsafeFrom(100)
    )

  private def generateSequence(
    p: Program.Id,
    o: Observation.Id
  ): IO[List[Atom[GmosNorth]]] =
    withServices(serviceUser): services =>
      services.generator.generate(p, o).map: e =>
        val g =
          e.toOption
           .flatMap: iec =>
             InstrumentExecutionConfig.gmosNorth.getOption(iec)
           .get.executionConfig.science.get
        g.nextAtom :: g.possibleFuture

  private def writeSequence(
    o: Observation.Id,
    s: List[Atom[GmosNorth]]
  ): IO[Unit] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          sequenceService
            .insertGmosNorthSequence(o, SequenceType.Science, none, Stream.emits(s))

  private def readSequence(
    o: Observation.Id
  ): IO[List[Atom[GmosNorth]]] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          sequenceService
            .streamGmosNorthSequence(o, SequenceType.Science, GmosNorthStatic)
            .compile
            .toList

  test("exercise serialization"):
    val res = for
      p  <- createProgram
      t  <- createTargetWithProfileAs(pi, p)
      o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

      sn <- generateSequence(p, o)
      b  <- IO.realTimeInstant
      _  <- writeSequence(o, sn)
      e0 <- IO.realTimeInstant
      so <- readSequence(o)
      e1 <- IO.realTimeInstant

//      _  <- IO.println(s"WRITE TIME: ${java.time.Duration.between(b,  e0)}")
//      _  <- IO.println(s"READ TIME.: ${java.time.Duration.between(e0, e1)}")
//      _  <- IO.println(s"SEQUENCE..: ${so.mkString("\n")}")
    yield (sn, so)

    assertIOBoolean(res.map((in, out) => in == out))
