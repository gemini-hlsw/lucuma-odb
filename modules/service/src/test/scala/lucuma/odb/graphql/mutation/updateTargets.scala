// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.literal._
import lucuma.core.model.User

class updateTargets extends OdbSuite
                            with CreateProgramOps
                            with CreateObservationOps {

  import createTarget.FullTargetGraph                            

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("no updates") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {}
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateTargets" : {
                "targets" : [
                  {
                    "id" : $tid
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
  
  test("update name") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {
                name: "new name"
              }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateTargets" : {
                "targets" : [
                  {
                    "id" : $tid,
                    "name" : "new name"
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

  test("update name to null (fails)") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {
                name: null
              }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
                name
              }
            }
          }
        """,
        expected = Left(List("Argument 'input.SET.name' is invalid: cannot be null"))
       )          
      }    
    }
  }

  test("update tracking (none to sidereal)") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {
                sidereal: {
                  ra: { degrees: 42 }                  
                }
              }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
                sidereal {
                  ra { degrees }
                  dec { degrees }
                  epoch            
                  properMotion {
                    ra { milliarcsecondsPerYear }
                    dec { milliarcsecondsPerYear }
                  }
                  parallax {
                    milliarcseconds
                  }
                  catalogInfo {
                    name
                    id
                    objectType
                  }
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateTargets" : {
                "targets" : [
                  {
                    "id" : "t-103",
                    "sidereal" : {
                      "ra" : {
                        "degrees" : 42.0
                      },
                      "dec" : {
                        "degrees" : 0.0
                      },
                      "epoch" : "J2000.000",
                      "properMotion" : null,
                      "parallax" : null,
                      "catalogInfo" : null
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