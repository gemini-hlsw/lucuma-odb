// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.User

class createObservation_Flamingos2Imaging extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("create Flamingos2 imaging"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 100.0
                      at: { nanometers: 500.0 }
                    }
                  }
                }
                observingMode: {
                  flamingos2Imaging: {
                    filters: [
                      { filter: Y },
                      { filter: J }
                    ]
                    explicitReadMode: BRIGHT
                    explicitDecker: IMAGING
                    explicitReadoutMode: SCIENCE
                  }
                }
              }
            }) {
              observation {
                instrument
                observingMode {
                  mode
                  flamingos2Imaging {
                    filters { filter }
                    initialFilters { filter }
                    explicitReadMode
                    decker
                    defaultDecker
                    explicitDecker
                    readoutMode
                    defaultReadoutMode
                    explicitReadoutMode
                  }
                }
              }
            }
          }
        """,
        json"""
          {
            "createObservation": {
              "observation": {
                "instrument": "FLAMINGOS2",
                "observingMode": {
                  "mode": "FLAMINGOS_2_IMAGING",
                  "flamingos2Imaging": {
                    "filters": [
                      { "filter": "Y" },
                      { "filter": "J" }
                    ],
                    "initialFilters": [
                      { "filter": "Y" },
                      { "filter": "J" }
                    ],
                    "explicitReadMode": "BRIGHT",
                    "decker": "IMAGING",
                    "defaultDecker": "IMAGING",
                    "explicitDecker": "IMAGING",
                    "readoutMode": "SCIENCE",
                    "defaultReadoutMode": "SCIENCE",
                    "explicitReadoutMode": "SCIENCE"
                  }
                }
              }
            }
          }
        """.asRight)
