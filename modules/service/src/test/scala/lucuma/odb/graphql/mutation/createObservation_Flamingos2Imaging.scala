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
                    defaultReadMode
                    explicitReadMode
                    defaultReads
                    explicitReads
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
                    "defaultReadMode": "FAINT",
                    "explicitReadMode": "BRIGHT",
                    "defaultReads": "READS_1",
                    "explicitReads": null,
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

  test("create Flamingos2 imaging with arbitrary offsets"):
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
                      { filter: Y }
                    ]
                    explicitSpatialOffsets: [
                      { p: { microarcseconds:  10000000 }, q: { microarcseconds:         0 } },
                      { p: { microarcseconds:         0 }, q: { microarcseconds: -10000000 } }
                    ]
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  flamingos2Imaging {
                    spatialOffsets {
                      p { microarcseconds }
                      q { microarcseconds }
                    }
                    explicitSpatialOffsets {
                      p { microarcseconds }
                      q { microarcseconds }
                    }
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
                "observingMode": {
                  "flamingos2Imaging": {
                    "spatialOffsets": [
                      { "p": { "microarcseconds": 10000000 }, "q": { "microarcseconds": 0 } },
                      { "p": { "microarcseconds": 0 }, "q": { "microarcseconds": -10000000 } }
                    ],
                    "explicitSpatialOffsets": [
                      { "p": { "microarcseconds": 10000000 }, "q": { "microarcseconds": 0 } },
                      { "p": { "microarcseconds": 0 }, "q": { "microarcseconds": -10000000 } }
                    ]
                  }
                }
              }
            }
          }
        """.asRight)
