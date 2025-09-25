// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target

class observation extends OdbSuite {

  val pi  = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  test("can select defaultXBin for obs with multiple targets") {

    def createObservation(pid: Program.Id, tid1: Target.Id, tid2: Target.Id) =
      query(
        user = pi,
        query = s"""
          mutation {
            createObservation(
              input: {
                programId: "$pid"
                SET: {
                  constraintSet: {
                    cloudExtinction: POINT_ONE
                    imageQuality: POINT_ONE
                    skyBackground: DARKEST
                  }
                  targetEnvironment: { asterism: ["$tid1", "$tid2"] }
                  scienceRequirements: {
                    exposureTimeMode: {
                      signalToNoise: {
                        value: 100.0
                        at: { nanometers: 500 }
                      }
                    }
                    spectroscopy: {
                      wavelength: { nanometers: 500 }
                      resolution: 100
                      wavelengthCoverage: { nanometers: 20 }
                      focalPlane: SINGLE_SLIT
                      focalPlaneAngle: { microarcseconds: 0 }
                    }
                  }
                  observingMode: {
                    gmosNorthLongSlit: {
                      grating: R831_G5302
                      filter: R_PRIME
                      fpu: LONG_SLIT_0_50
                      centralWavelength: { nanometers: 500 }
                    }
                  }
                }
              }
            ) {
              observation {
                id
              }
            }
          }
        """
      ).map{ j => j.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id] }

    val anotherSourceProfile =
      """
        sourceProfile: {
          point: {
            bandNormalized: {
              sed: {
                stellarLibrary: O5_V
              }
              brightnesses: []
            }
          }
        }
      """
    val mkStuff: IO[Observation.Id] =
      for {
        pid  <- createProgramAs(pi)
        tid1 <- createTargetAs(pi, pid)
        tid2 <- createTargetAs(pi, pid, sourceProfile = anotherSourceProfile)
        oid  <- createObservation(pid, tid1, tid2)
       } yield oid

    mkStuff.flatMap { oid =>
      expect(
        user = pi,
        query =
          s"""
            query {
              observation(observationId: "$oid") {
                useBlindOffset
                observingMode {
                  gmosNorthLongSlit {
                    defaultXBin
                  }
                }
                targetEnvironment {
                  asterism {
                    id
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
              "observation" : {
                "useBlindOffset" : false,
                "observingMode" : {
                  "gmosNorthLongSlit" : {
                    "defaultXBin" : "ONE"
                  }
                },
                "targetEnvironment" : {
                  "asterism" : [
                    {
                      "id" : "t-194"
                    },
                    {
                      "id" : "t-195"
                    }
                  ]
                }
              }
            }
          """
        )
      )
    }


  }

}
