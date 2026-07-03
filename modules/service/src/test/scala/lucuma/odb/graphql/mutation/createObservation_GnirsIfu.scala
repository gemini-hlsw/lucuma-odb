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
                        ifu: { fpu: LOW_RESOLUTION }
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
                        slit { fpu }
                        ifu { fpu initialFpu }
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
                      "slit": null,
                      "ifu": {
                        "fpu": "LOW_RESOLUTION",
                        "initialFpu": "LOW_RESOLUTION"
                      },
                      "decker": "LOW_RESOLUTION_IFU",
                      "defaultDecker": "LOW_RESOLUTION_IFU"
                    }
                  }
                }
              }
            }
          """)
        )

  test("create GNIRS IFU rejects providing both slit and ifu"):
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        ifu: { fpu: LOW_RESOLUTION }
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                      }
                    }
                  }
                }) { observation { id } }
              }
            """,
          expected = Left(List("Argument 'input.SET.observingMode.gnirsSpectroscopy' is invalid: Only one of 'slit' or 'ifu' may be provided."))
        )

  test("GNIRS IFU default telescope configs (extended source); slit configs null"):
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
                        ifu: { fpu: LOW_RESOLUTION }
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        exposureTimeMode: {
                          timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2200 } }
                        }
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      gnirsSpectroscopy {
                        slit { telescopeConfigs { offsetMode } }
                        ifu { telescopeConfigs { offset { p { microarcseconds } q { microarcseconds } } guiding } }
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
                    "gnirsSpectroscopy": {
                      "slit": null,
                      "ifu": {
                        "telescopeConfigs": [
                          { "offset": { "p": { "microarcseconds": 150000 },    "q": { "microarcseconds": 150000 } },    "guiding": "ENABLED" },
                          { "offset": { "p": { "microarcseconds": 10000000 },  "q": { "microarcseconds": 10000000 } },  "guiding": "DISABLED" },
                          { "offset": { "p": { "microarcseconds": -150000 },   "q": { "microarcseconds": -150000 } },   "guiding": "ENABLED" },
                          { "offset": { "p": { "microarcseconds": -10000000 }, "q": { "microarcseconds": -10000000 } }, "guiding": "DISABLED" }
                        ]
                      }
                    }
                  }
                }
              }
            }
          """)
        )

  test("create GNIRS IFU rejects slit configs provided alongside ifu"):
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
                        ifu: { fpu: LOW_RESOLUTION }
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        slit: { explicitTelescopeConfigs: { toSky: [ { offset: { p: { arcseconds: 1 }, q: { arcseconds: 1 } }, guiding: ENABLED } ] } }
                      }
                    }
                  }
                }) { observation { id } }
              }
            """,
          expected = Left(List("Argument 'input.SET.observingMode.gnirsSpectroscopy' is invalid: Only one of 'slit' or 'ifu' may be provided."))
        )

  test("update GNIRS IFU rejects slit telescope configs when the FPU is unchanged"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createGnirsIfuObservationAs(pi, pid, tid)
      _   <- expect(
        user  = pi,
        query =
          s"""
            mutation {
              updateObservations(input: {
                SET: {
                  observingMode: {
                    gnirsSpectroscopy: {
                      slit: { explicitTelescopeConfigs: { toSky: [ { offset: { p: { arcseconds: 1 }, q: { arcseconds: 1 } }, guiding: ENABLED } ] } }
                    }
                  }
                }
                WHERE: { id: { EQ: "$oid" } }
              }) { observations { id } }
            }
          """,
        expected = Left(List("'slit.explicitTelescopeConfigs' is only valid with a long-slit FPU."))
      )
    yield ()
