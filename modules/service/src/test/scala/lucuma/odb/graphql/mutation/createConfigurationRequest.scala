// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class createConfigurationRequest extends OdbSuite with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val admin   = TestUsers.Standard.admin(2, 32)

  val validUsers = List(pi, admin).toList

  test("create and select configuration request for fully-configured observation") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                mutation {
                  createConfigurationRequest(input: {
                    observationId: "$oid"
                    SET: {
                      justification: "Because I said so."
                    }
                  }) {
                    status
                    justification
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra { 
                            hms 
                          }
                          dec { 
                            dms 
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "createConfigurationRequest" : {
                    "status" : "REQUESTED",
                    "justification" : "Because I said so.",
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_ONE",
                        "cloudExtinction" : "POINT_ONE",
                        "skyBackground" : "DARKEST",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : {
                          "ra" : {
                            "hms" : "05:46:13.138550"
                          },
                          "dec" : {
                            "dms" : "-00:06:04.916777"
                          }
                        }
                      },
                      "observingMode" : {
                        "instrument" : "GMOS_NORTH",
                        "mode" : "GMOS_NORTH_LONG_SLIT",
                        "gmosNorthLongSlit" : {
                          "grating" : "R831_G5302"
                        },
                        "gmosSouthLongSlit" : null
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }

  test("can't create configuration request if no targets") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createObservationAs(pi, pid).flatMap { oid =>
          expectOdbError(
            user = pi,
            query = s"""
              mutation {
                createConfigurationRequest(input: {
                  observationId: "$oid"
                }) {
                  id
                }
              }
            """,
            expected = {
              case OdbError.GuideEnvironmentError(_) => // expected
              case OdbError.InvalidConfiguration(_)  => // expected
            }
          )
        }
      }
    }
  }

  test("can't create configuration request if no CFP") {
      createProgramAs(pi).flatMap { pid =>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expectOdbError(
              user = pi,
              query = s"""
                mutation {
                  createConfigurationRequest(input: {
                    observationId: "$oid"
                  }) {
                    id
                  }
                }
              """,
              expected = {
                case OdbError.GuideEnvironmentError(_) => // expected
                case OdbError.InvalidConfiguration(_)  => // expected
              }
            )
          }
        }
    }
  }

  test("can't create configuration request if no observing mode") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createObservationAs(pi, pid, tid).flatMap { oid =>
            expectOdbError(
              user = pi,
              query = s"""
                mutation {
                  createConfigurationRequest(input: {
                    observationId: "$oid"
                  }) {
                    id
                  }
                }
              """,
              expected = {
                case OdbError.GuideEnvironmentError(_) => // expected
                case OdbError.InvalidConfiguration(_)  => // expected
              }
            )
          }
        }
      }
    }
  }

  test("identical requests are canonicalized") {
    for
        cfpid <- createCallForProposalsAs(admin)
        pid   <- createProgramAs(pi, "Foo")
        _     <- addProposal(pi, pid, Some(cfpid), None)
        tid   <- createTargetWithProfileAs(pi, pid)
        oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        req1  <- createConfigurationRequestAs(pi, oid)
        req2  <- createConfigurationRequestAs(pi, oid)
      yield assert(req1 === req2)    
  }

  test("identical requests are canonicalized, even if justifications differ") {
    for
        cfpid <- createCallForProposalsAs(admin)
        pid   <- createProgramAs(pi, "Foo")
        _     <- addProposal(pi, pid, Some(cfpid), None)
        tid   <- createTargetWithProfileAs(pi, pid)
        oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        req1  <- createConfigurationRequestAs(pi, oid, NonEmptyString.from("j1").toOption)
        req2  <- createConfigurationRequestAs(pi, oid, NonEmptyString.from("j1").toOption)
      yield assert(req1 === req2)    
  }

}
