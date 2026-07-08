// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Program

class observingModeGroup_GhostIfu extends OdbSuite with ObservingModeSetupOperations:

  val pi         = TestUsers.Standard.pi(nextId, nextId)
  val staff      = TestUsers.Standard.staff(nextId, nextId)
  val validUsers = List(pi, staff)

  private def groupQuery(pid: Program.Id): String =
    s"""
      query {
        observingModeGroup(programId: ${pid.asJson}) {
          matches {
            observingMode {
              ghostIfu { resolutionMode }
            }
            observations {
              matches { id }
            }
          }
        }
      }
    """

  // base grouping .
  test("GHOST observations sharing a mode should be grouped together"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid, "Biff").flatMap: tid =>
        // Created sequentially so the ids come back in creation (== ascending) order.
        createGhostIfuObservationAs(pi, pid, List(tid)).replicateA(2).flatMap: obs =>
          expect(
            user  = pi,
            query = groupQuery(pid),
            expected =
              json"""
                {
                  "observingModeGroup" : {
                    "matches" : [
                      {
                        "observingMode" : {
                          "ghostIfu" : {
                            "resolutionMode" : "STANDARD"
                          }
                        },
                        "observations" : {
                          "matches" : ${obs.map { id => Json.obj("id" -> id.asJson) }.asJson}
                        }
                      }
                    ]
                  }
                }
              """.asRight
          )

  // distinct configs form distinct groups.
  test("GHOST observations with distinct resolution modes form distinct groups"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid, "Biff").flatMap: tid =>
        for
          oStandard <- createGhostIfuObservationAs(pi, pid, List(tid), resolutionMode = "STANDARD")
          oHigh     <- createGhostIfuObservationAs(pi, pid, List(tid), resolutionMode = "HIGH")
          _         <- expect(
            user  = pi,
            query = groupQuery(pid),
            expected =
              json"""
                {
                  "observingModeGroup" : {
                    "matches" : [
                      {
                        "observingMode" : {
                          "ghostIfu" : {
                            "resolutionMode" : "HIGH"
                          }
                        },
                        "observations" : {
                          "matches" : ${List(Json.obj("id" -> oHigh.asJson)).asJson}
                        }
                      },
                      {
                        "observingMode" : {
                          "ghostIfu" : {
                            "resolutionMode" : "STANDARD"
                          }
                        },
                        "observations" : {
                          "matches" : ${List(Json.obj("id" -> oStandard.asJson)).asJson}
                        }
                      }
                    ]
                  }
                }
              """.asRight
          )
        yield ()

  test("GHOST observations edited to share a mode should merge into one group"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid, "Biff").flatMap: tid =>
        for
          o1 <- createGhostIfuObservationAs(pi, pid, List(tid), resolutionMode = "STANDARD")
          o2 <- createGhostIfuObservationAs(pi, pid, List(tid), resolutionMode = "HIGH")
          _  <- query(
            user  = pi,
            query = s"""
              mutation {
                updateObservations(input: {
                  SET: {
                    observingMode: {
                      ghostIfu: { resolutionMode: STANDARD }
                    }
                  }
                  WHERE: { id: { EQ: "${o2.toString}" } }
                }) {
                  observations { id }
                }
              }
            """
          )
          _  <- expect(
            user  = pi,
            query = groupQuery(pid),
            expected =
              json"""
                {
                  "observingModeGroup" : {
                    "matches" : [
                      {
                        "observingMode" : {
                          "ghostIfu" : {
                            "resolutionMode" : "STANDARD"
                          }
                        },
                        "observations" : {
                          "matches" : ${List(o1, o2).map { id => Json.obj("id" -> id.asJson) }.asJson}
                        }
                      }
                    ]
                  }
                }
              """.asRight
          )
        yield ()
