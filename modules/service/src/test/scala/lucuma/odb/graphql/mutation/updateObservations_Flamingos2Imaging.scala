// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.model.User

class updateObservations_Flamingos2Imaging extends OdbSuite with UpdateObservationsOps:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("observing mode: setting Flamingos2 imaging persists the mode"):
    val update = """
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
    """

    val query = """
      observations {
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
    """

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
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
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
