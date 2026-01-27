// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.Services.Syntax.*

import java.time.Duration

class SequenceServiceSuite extends ExecutionTestSupportForGmos:

  override val FakeItcResult: IntegrationTime =
    IntegrationTime(
      10.secTimeSpan,
      PosInt.unsafeFrom(100)
    )

  test("exercise serialization"):
    val time = for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      s <- withServices(serviceUser): services =>
             services.generator.generate(p, o).map: e =>
               val g = e.toOption
                 .flatMap: iec =>
                   InstrumentExecutionConfig.gmosNorth.getOption(iec)
                 .get.executionConfig.science.get
               g.nextAtom :: g.possibleFuture
      b <- IO.realTimeInstant
      _ <- withServices(serviceUser): services =>
             services
               .transactionally:
                 sequenceService
                   .insertGmosNorthSequence(o, SequenceType.Science, none, Stream.emits(s))
      e <- IO.realTimeInstant
    yield Duration.between(b, e)

    assertIOBoolean(time.flatTap(d => IO.println(s"Duration: $d")).map(d => d.compareTo(Duration.ofSeconds(10L)) < 0))
