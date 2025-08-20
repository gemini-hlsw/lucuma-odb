// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.Ior
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CallForProposalsType.DemoScience
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SkyBackground
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.model.User

class constraintSetGroup extends OdbSuite {

  val pi         = TestUsers.Standard.pi(nextId, nextId)
  val staff      = TestUsers.Standard.staff(nextId, nextId)
  val validUsers = List(pi, staff)

  def createObservation(user: User, pid: Program.Id, iq: ImageQuality.Preset, sb: SkyBackground): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
            programId: ${pid.asJson},
              SET: {
                constraintSet: {
                  imageQuality: ${iq.tag.toUpperCase}
                  skyBackground: ${sb.tag.toUpperCase}
                }
              }
            }) {
              observation {
                id
              }
            }
          }
        """
    ).map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  test("constraints should be correctly grouped") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        def create2(iq: ImageQuality.Preset, sb: SkyBackground) = createObservation(user, pid, iq, sb).replicateA(2)
        (
          create2(ImageQuality.Preset.OnePointFive, SkyBackground.Bright),
          create2(ImageQuality.Preset.PointOne, SkyBackground.Bright),
          create2(ImageQuality.Preset.PointOne, SkyBackground.Dark)
        ).parTupled.flatMap { (g1, g2, g3) =>
          expect(
            user = user,
            query =
              s"""
              query {
                constraintSetGroup(programId: ${pid.asJson}) {
                  matches {
                    constraintSet {
                      imageQuality
                      skyBackground
                    }
                    observations {
                      matches {
                        id
                      }
                    }
                  }
                }
              }
              """,
            expected = Right(
              // N.B. the ordering of groups is based on the concatenation of all the components so it's deterministic
              json"""
                {
                  "constraintSetGroup" : {
                    "matches" : [
                      {
                        "constraintSet" : {
                          "imageQuality" : "ONE_POINT_FIVE",
                          "skyBackground" : "BRIGHT"
                        },
                        "observations" : {
                          "matches" : ${g1.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      },
                      {
                        "constraintSet" : {
                          "imageQuality" : "POINT_ONE",
                          "skyBackground" : "BRIGHT"
                        },
                        "observations" : {
                          "matches" : ${g2.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      },
                      {
                        "constraintSet" : {
                          "imageQuality" : "POINT_ONE",
                          "skyBackground" : "DARK"
                        },
                        "observations" : {
                          "matches" : ${g3.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      }
                    ]
                  }
                }
              """
            )
          )
        }
      }
    }
  }

  test("should be able to use a proposal reference") {
    List(pi).traverse { user =>
      createProgramWithUsPi(user).flatMap { pid =>
        def create2(iq: ImageQuality.Preset, sb: SkyBackground) = 
          createObservation(user, pid, iq, sb)
            .flatTap(setObservationWorkflowState(user, _, ObservationWorkflowState.Inactive)) // avoid submission error
            .replicateA(2)
        (
          create2(ImageQuality.Preset.OnePointFive, SkyBackground.Bright),
          create2(ImageQuality.Preset.PointOne, SkyBackground.Bright),
          create2(ImageQuality.Preset.PointOne, SkyBackground.Dark)
        ).parTupled.flatMap { (g1, g2, g3) =>
          createCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A")).flatMap { cid =>
           addDemoScienceProposal(user, pid, cid)
          } *>
          submitProposal(user, pid) *>
          expectIor(
            user = user,
            query =
              s"""
              query {
                constraintSetGroup(proposalReference: "G-2025A-0001") {
                  matches {
                    constraintSet {
                      imageQuality
                      skyBackground
                    }
                    observations {
                      matches {
                        id
                      }
                    }
                  }
                }
              }
              """,
            expected = Ior.right(
              // N.B. the ordering of groups is based on the concatenation of all the components so it's deterministic
              json"""
                {
                  "constraintSetGroup" : {
                    "matches" : [
                      {
                        "constraintSet" : {
                          "imageQuality" : "ONE_POINT_FIVE",
                          "skyBackground" : "BRIGHT"
                        },
                        "observations" : {
                          "matches" : ${g1.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      },
                      {
                        "constraintSet" : {
                          "imageQuality" : "POINT_ONE",
                          "skyBackground" : "BRIGHT"
                        },
                        "observations" : {
                          "matches" : ${g2.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      },
                      {
                        "constraintSet" : {
                          "imageQuality" : "POINT_ONE",
                          "skyBackground" : "DARK"
                        },
                        "observations" : {
                          "matches" : ${g3.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      }
                    ]
                  }
                }
              """
            )
          )
        }
      }
    }
  }

}
