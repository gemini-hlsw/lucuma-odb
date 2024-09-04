// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.Json
import io.circe.literal.*
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.data.OdbError

class createConfigurationRequest extends OdbSuite with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val admin   = TestUsers.Standard.admin(2, 32)

  val validUsers = List(pi, admin).toList

  test("create and select configuration request for fully-configured observation") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None, "Foo") >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                mutation {
                  createConfigurationRequest(input: {
                    observationId: "$oid"
                  }) {
                    status
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      referenceCoordinates {
                        ra { 
                          hms 
                        }
                        dec { 
                          dms 
                        }
                      }
                #      observingMode {
                #        instrument
                #        mode
                #        gmosNorthLongSlit {
                #          grating
                #        }
                #        gmosSouthLongSlit {
                #          grating
                #        }
                #     }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "createConfigurationRequest" : {
                    "status" : "REQUESTED",
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_ONE",
                        "cloudExtinction" : "POINT_ONE",
                        "skyBackground" : "DARKEST",
                        "waterVapor" : "WET"
                      },
                      "referenceCoordinates" : {
                        "ra" : {
                          "hms" : "05:46:13.138550"
                        },
                        "dec" : {
                          "dms" : "-00:06:04.916777"
                        }
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
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None, "Foo") >>
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
              case OdbError.GuideEnvironmentError(Some(s"No targets have been defined for observation $oid.")) => // ok
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
                case OdbError.InvalidConfiguration(Some(s"Reference coordinates are not available.")) => // ok
              }
            )
          }
        }
    }
  }

  test("can't create configuration request if no observing mode") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None, "Foo") >>
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
                case OdbError.InvalidConfiguration(Some(s"Observing mode is undefined.")) => // ok
              }
            )
          }
        }
      }
    }
  }

}
