// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.literal.*
import lucuma.core.model.User

class createObservation_GnirsIfu extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi)

  test("create GNIRS IFU — fpuIfu round-trips, type is GNIRS_IFU, decker is IFU"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query =
            s"""
              mutation {
                createObservation(input: {
                  programId: "$pid"
                  SET: {
                    targetEnvironment: { asterism: [ "$tid" ] }
                    scienceRequirements: {
                      spectroscopy: {
                        wavelength: { nanometers: 2200 }
                        resolution: 1000
                        wavelengthCoverage: { nanometers: 200 }
                        focalPlane: IFU
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuIfu: LOW_RESOLUTION
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        exposureTimeMode: {
                          timeAndCount: {
                            time: { seconds: 30.0 }
                            count: 3
                            at: { nanometers: 2200 }
                          }
                        }
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      instrument
                      mode
                      gnirsSpectroscopy {
                        fpuSlit
                        fpuIfu
                        initialFpuSlit
                        initialFpuIfu
                        decker
                        defaultDecker
                      }
                    }
                  }
                }
              }
            """,
          expected = Right(json"""
            {
              "createObservation": {
                "observation": {
                  "observingMode": {
                    "instrument": "GNIRS",
                    "mode": "GNIRS_IFU",
                    "gnirsSpectroscopy": {
                      "fpuSlit": null,
                      "fpuIfu": "LOW_RESOLUTION",
                      "initialFpuSlit": null,
                      "initialFpuIfu": "LOW_RESOLUTION",
                      "decker": "LOW_RESOLUTION_IFU",
                      "defaultDecker": "LOW_RESOLUTION_IFU"
                    }
                  }
                }
              }
            }
          """)
        )

  test("create GNIRS IFU rejects providing both fpuSlit and fpuIfu"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query =
            s"""
              mutation {
                createObservation(input: {
                  programId: "$pid"
                  SET: {
                    targetEnvironment: { asterism: [ "$tid" ] }
                    scienceRequirements: {
                      spectroscopy: {
                        wavelength: { nanometers: 2200 }
                        resolution: 1000
                        wavelengthCoverage: { nanometers: 200 }
                        focalPlane: IFU
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        fpuIfu: LOW_RESOLUTION
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                      }
                    }
                  }
                }) { observation { id } }
              }
            """,
          expected = Left(List("Argument 'input.SET.observingMode.gnirsSpectroscopy' is invalid: Only one of 'fpuSlit' or 'fpuIfu' may be provided."))
        )
