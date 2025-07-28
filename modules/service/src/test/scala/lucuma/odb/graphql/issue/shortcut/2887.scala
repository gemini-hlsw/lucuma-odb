// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.logic.Generator.SequenceAtomLimit

class ShortCut_2887 extends ExecutionTestSupportForGmos {

  lazy val atomCount: Ref[IO, Int] =
    Ref.unsafe(0)

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      1.hrTimeSpan,
      PosInt.unsafeFrom(atomCount.get.unsafeRunSync() max 1)
    )

  test("forever sequence") {
    val setup: IO[Observation.Id] =
      for {
        _ <- atomCount.set(SequenceAtomLimit + 1)
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(serviceUser, p, List(t))
        _ <- runObscalcUpdate(p, o)
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
                     value {
                       science {
                         observeClass
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          List(s"Could not generate a sequence for $oid: The generated sequence is too long (more than ${SequenceAtomLimit} atoms).").asLeft
      )
    }
  }

  test("on the edge of forever") {
    val setup: IO[Observation.Id] =
      for {
        _ <- atomCount.set(SequenceAtomLimit - 1)
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
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
                     value {
                       science {
                         observeClass
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "digest": {
                    "value": {
                      "science" : {
                        "observeClass" : "SCIENCE"
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
    }
  }
}
