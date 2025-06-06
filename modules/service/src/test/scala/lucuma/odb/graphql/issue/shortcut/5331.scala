// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import grackle.Problem
import grackle.Result
import grackle.Value
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.LongBinding
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class ShortCut_5331 extends ExecutionTestSupportForGmos:
  // Works up to 2147 seconds
  val secs: Long = 2148000000L

  // RCN: the test below will never work because 2148000000 is syntactically invalid GraphQL

  // def updateExpTime(t: Observation.Id) = query(pi,
  //   s"""
  //     mutation {
  //       updateObservations(input: {
  //         SET: {
  //           scienceRequirements: {
  //             spectroscopy: {
  //               exposureTimeMode: {
  //                 timeAndCount: {
  //                   time: {
  //                     microseconds: $secs
  //                   },
  //                   count: 1,
  //                   at: {
  //                     nanometers: 500
  //                   }
  //                 }
  //               }
  //             }
  //           }
  //         }
  //         WHERE: {
  //           id: { EQ: "$t"}
  //         }
  //       }) {
  //         observations {
  //           id
  //         }
  //       }
  //     }
  //   """)


  // test("Support passing large exposure time values in graphql"):
  //   val setup =
  //     for {
  //       p <- createProgram
  //       t <- createTargetWithProfileAs(pi, p)
  //       o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
  //       _ <- updateExpTime(o)
  //     } yield o
  //   setup.flatMap { case oid =>
  //     expect(
  //       user  = pi,
  //       query =
  //         s"""
  //            query {
  //              observation(observationId: "$oid") {
  //                scienceRequirements {
  //                  spectroscopy {
  //                    exposureTimeMode {
  //                      timeAndCount {
  //                        time {
  //                          microseconds
  //                        }
  //                      }
  //                    }
  //                  }
  //                }
  //              }
  //            }
  //          """,
  //           expected = Right(json"""
  //             {
  //               "observation" : {
  //                 "scienceRequirements" : {
  //                   "spectroscopy" : {
  //                     "exposureTimeMode" : {
  //                       "timeAndCount" : {
  //                         "time" : {
  //                           "microseconds" : $secs
  //                         }
  //                       }
  //                     }
  //                   }
  //                 }
  //               }
  //             }
  //           """)
  //     )
  //   }

  def updateExpTimeWithVariables(t: Observation.Id) = query(pi,
    s"""
      mutation($$input: UpdateObservationsInput!) {
        updateObservations(input: $$input) {
          observations {
            id
          }
        }
      }
    """,
    variables =
      json"""
        {
          "input": {
            "SET": {
              "scienceRequirements": {
                "exposureTimeMode": {
                    "timeAndCount": {
                      "time": {
                        "microseconds": $secs
                      },
                      "count": 1,
                      "at": {
                        "nanometers": 500
                      }
                    }
                  }
                }
              }
            },
            "WHERE": {
              "id": { "EQ": $t }
            }
        }
      """.asObject
  )

  test("Support passing large exposure time values in variables"):
    val setup =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- updateExpTimeWithVariables(o)
      } yield o
    setup.flatMap { case oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 scienceRequirements {
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
           """,
          expected = Right(json"""
            {
              "observation" : {
                "scienceRequirements" : {
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
          """)
      )
    }


  List(Long.MinValue, 0L, Long.MaxValue).foreach: long =>
    test(s"LongBinding ($long)"):
      assertEquals(
        LongBinding.unapply(("test", Value.FloatValue(long.toDouble))),
        Some(("test", Result(long)))
      )

  test(s"LongBinding (1.23)"):
    assertEquals(
      LongBinding.unapply(("test", Value.FloatValue(1.23))),
      Some(("test", Result.failure(
        Problem(
          "Argument 'test' is invalid: Expected Long, got FloatValue(1.23)",
          Nil,
          Nil,
          Some(
            json"""
              {
                "odb_error" : {
                  "odb_error.tag" : "invalid_argument",
                  "odb_error.detail" : "Argument 'test' is invalid: Expected Long, got FloatValue(1.23)",
                  "odb_error.data" : { }
                }
              }
            """.asObject.get
          )
        )
      )))
    )
