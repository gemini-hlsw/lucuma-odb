// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.logic.Generator.SequenceAtomLimit

class ShortCut_2887 extends ExecutionTestSupport {

  val atomCount: Ref[IO, Int] =
    Ref.unsafe(0)

  override def fakeItcResult: IntegrationTime =
    IntegrationTime(
      1.secTimeSpan,
      PosInt.unsafeFrom(atomCount.get.unsafeRunSync()),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  test("forever sequence") {
    val setup: IO[Observation.Id] =
      for {
        _ <- atomCount.set(SequenceAtomLimit - 1) // 2 more for the arcs puts it over
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(serviceUser, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     science {
                       observeClass
                     }
                   }
                 }
               }
             }
           """,
        expected = List(s"The generated sequence is too long (more than ${SequenceAtomLimit} atoms).").asLeft
      )
    }
  }

  test("on the edge of forever") {
    val setup: IO[Observation.Id] =
      for {
        _ <- atomCount.set(SequenceAtomLimit - 2)
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     science {
                       observeClass
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "digest": {
                    "science" : {
                      "observeClass" : "SCIENCE"
                    }
                  }
                }
              }
            }
          """
        )
      )
    }
  }
}