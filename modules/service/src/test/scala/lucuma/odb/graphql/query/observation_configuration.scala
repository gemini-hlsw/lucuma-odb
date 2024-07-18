// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import io.circe.Json
import io.circe.literal.*

class observation_configuration extends OdbSuite with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val admin   = TestUsers.Standard.admin(2, 32)

  val validUsers = List(pi, admin).toList

  test("select configuration for fully-configured observation") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None, "Foo") >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
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
                  "observation" : {
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

}
