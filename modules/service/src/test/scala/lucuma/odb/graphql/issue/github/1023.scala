// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import cats.effect.IO
import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class GitHub_1023 extends ExecutionTestSupportForGmos {

  test("one inline fragment") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi, "Interface Issue")
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
                   config(futureLimit: 100) {
                     gmosNorth {
                       static {
                         stageMode
                         detector
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
              "observation" : {
                "execution" : {
                  "config" : {
                    "gmosNorth": {
                      "static" : {
                        "stageMode" : "FOLLOW_XY",
                        "detector" : "HAMAMATSU"
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

  test("two inline fragments") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi, "Interface Issue")
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
                   config(futureLimit: 100) {
                     instrument
                     gmosNorth {
                       static {
                         stageMode
                         detector
                       }
                     }
                     gmosSouth {
                       static {
                         stageMode
                         detector
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
              "observation" : {
                "execution" : {
                  "config" : {
                    "instrument": "GMOS_NORTH",
                    "gmosNorth": {
                      "static" : {
                        "stageMode" : "FOLLOW_XY",
                        "detector" : "HAMAMATSU"
                      }
                    },
                    "gmosSouth": null
                  }
                }
              }
            }
          """.asRight
        )
    }
  }
}