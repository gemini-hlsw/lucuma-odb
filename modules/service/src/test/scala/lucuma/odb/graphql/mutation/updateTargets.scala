// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.literal._
import lucuma.core.model.User

class updateTargets extends OdbSuite with CreateProgramOps with CreateObservationOps {

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

  test("update tracking (sidereal -> sidereal)") {
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
                    "id" : $tid,
                    "sidereal" : {
                      "ra" : {
                        "degrees" : 42.0
                      }
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

  test("update tracking (sidereal -> nonsidereal)") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {
                nonsidereal: {
                  keyType: COMET
                  des: "foo"
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
                }
                nonsidereal {
                  keyType
                  des
                  key
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
                    "id" : $tid,
                    "sidereal" : null,
                    "nonsidereal" : {
                      "keyType" : "COMET",
                      "des" : "foo",
                      "key" : "Comet_foo"
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

  // This one works correctly but someone somewhere (I suspect cats-effect Resource) is dumping a
  // stacktrace for the constraint failure that we handle. So I want to track that down to avoid
  // a bunch of worrying text in the test output.
  test("update tracking (nonsidereal -> sidereal, incomplete)".ignore) {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
        // first change to nonsidereal
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  nonsidereal: {
                    keyType: COMET
                    des: "foo"
                  }
                }
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
        ) *>
        // and now change back, but don't define everything
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sidereal: {
                    ra: { degrees: 0 }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sidereal {
                    ra { degrees }
                    dec { degrees }
                    epoch
                  }
                  nonsidereal {
                    keyType
                    des
                  }
                }
              }
            }
          """,
          expected = Left(List("Sidereal targets require RA, Dec, and Epoch to be defined."))
        )          
      }    
    }
  }

  test("update tracking (nonsidereal -> sidereal)") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
        // first change to nonsidereal
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  nonsidereal: {
                    keyType: COMET
                    des: "foo"
                  }
                }
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
        ) *>
        // and now change back
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sidereal: {
                    ra: { degrees: 12 }
                    dec: { degrees: 67 }
                    epoch: "J1997.234"
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sidereal {
                    ra { degrees }
                    dec { degrees }
                    epoch
                  }
                  nonsidereal {
                    keyType
                    des
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
                      "sidereal" : {
                        "ra" : {
                          "degrees" : 12.0
                        },
                        "dec" : {
                          "degrees" : 67.0
                        },
                        "epoch" : "J1997.234"
                      },
                      "nonsidereal" : null
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

  test("update source profile (point/bandNormalized/sed)") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    point: {
                      bandNormalized: {
                        sed: {
                          planetaryNebula: NGC7009
                        }
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    point {
                      bandNormalized {
                        sed {
                          planetaryNebula
                        }
                      }
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
                      "sourceProfile" : {
                        "point" : {
                          "bandNormalized" : {
                            "sed" : {
                              "planetaryNebula" : "NGC7009"
                            }
                          }
                        }
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

  test("update source profile (point -> gaussian, incomplete)") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    gaussian: {
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      fwhm { degrees }
                    }
                    point {
                      bandNormalized {
                        sed {
                          planetaryNebula
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Left(List("Not a gaussian source.  To change profile type, please provide a full definition."))
        )          
      }    
    }
  }

  test("update source profile (point -> gaussian, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createEmptyTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    gaussian: {
                      fwhm: { degrees: 42 }
                      spectralDefinition: {
                        emissionLines: {
                          lines: []
                          fluxDensityContinuum: {
                            value: 1.23
                            error: 0.0001
                            units: W_PER_M_SQUARED_PER_UM
                          }
                        }
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      fwhm { degrees }
                      emissionLines {
                        fluxDensityContinuum {
                          value
                          units
                          error
                        }
                      }
                    }
                    point {
                      bandNormalized {
                        sed {
                          planetaryNebula
                        }
                      }
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
                      "sourceProfile" : {
                        "gaussian" : {
                          "fwhm" : {
                            "degrees" : 42.000000
                          },
                          "emissionLines" : {
                            "fluxDensityContinuum" : {
                              "value" : "1.23",
                              "units" : "W_PER_M_SQUARED_PER_UM",
                              "error" : "0.00010"
                            }
                          }
                        },
                        "point" : null
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