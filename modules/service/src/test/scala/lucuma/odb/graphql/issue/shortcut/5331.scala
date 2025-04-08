// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.odb.graphql.query.ExecutionTestSupport

class ShortCut_5331 extends ExecutionTestSupport:
  // Works up to 2147 seconds
  val secs: Long = 2148000000L

  def updateExpTime(t: Observation.Id) = query(pi,
    s"""
      mutation {
        updateObservations(input: {
          SET: {
            scienceRequirements: {
              spectroscopy: {
                exposureTimeMode: {
                  timeAndCount: {
                    time: {
                      microseconds: $secs
                    },
                    count: 1,
                    at: {
                      nanometers: 500
                    }
                  }
                }
              }
            }
          }
          WHERE: {
            id: { EQ: "$t"}
          }
        }) {
          observations {
            id
          }
        }
      }
    """)

  test("Support passing large exposure time values"):
    val setup =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- updateExpTime(o)
      } yield o
    setup.flatMap { case oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 scienceRequirements {
                   spectroscopy {
                     exposureTimeMode {
                       timeAndCount {
                         time {
                           microseconds
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
            expected = Right(json"""
              {
                "observation" : {
                  "scienceRequirements" : {
                    "spectroscopy" : {
                      "exposureTimeMode" : {
                        "timeAndCount" : {
                          "time" : {
                            "microseconds" : $secs
                          }
                        }
                      }
                    }
                  }
                }
              }
            """)
      )
    }
