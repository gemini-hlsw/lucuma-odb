// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class constraintSetGroup extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  def createProgram(user: User): IO[Program.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createProgram(input: {}) {
              program {
                id
              }
            }
          }
        """
    ) map { json =>
      json.hcursor.downFields("createProgram", "program", "id").require[Program.Id]
    }

  def createObservation(user: User, pid: Program.Id, iq: ImageQuality, sb: SkyBackground): IO[Observation.Id] =
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
      createProgram(user).flatMap { pid =>
        def create2(iq: ImageQuality, sb: SkyBackground) = createObservation(user, pid, iq, sb).replicateA(2)
        (
          create2(ImageQuality.OnePointFive, SkyBackground.Bright), 
          create2(ImageQuality.PointOne, SkyBackground.Bright),
          create2(ImageQuality.PointOne, SkyBackground.Dark)
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


}