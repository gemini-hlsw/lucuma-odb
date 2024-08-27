// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.Json
import io.circe.literal.*
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class createConfigurationRequest extends OdbSuite with ObservingModeSetupOperations {

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
                mutation {
                  createConfigurationRequest(input: {
                    observationId: "$oid"
                  }) {
                    id
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
                42
              """)
            )
          }
        }
      }
    }
  }

}
