// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.eq.*
import io.circe.literal.*
import lucuma.core.model.User
import lucuma.odb.data.OdbError

class createObservation_GnirsLongSlit extends OdbSuite:

  val pi:    User = TestUsers.Standard.pi(nextId, nextId)
  val staff: User = TestUsers.Standard.staff(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi, staff)

  test("create GNIRS Long Slit with required fields — defaults computed"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
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
                        grating
                        initialGrating
                        prism
                        initialPrism
                        camera
                        initialCamera
                        fpuSlit
                        initialFpuSlit
                        filter
                        initialFilter
                        coadds
                        decker
                        defaultDecker
                        explicitDecker
                        centralWavelength { nanometers }
                        initialCentralWavelength { nanometers }
                        explicitReadMode
                        wellDepth
                        defaultWellDepth
                        explicitWellDepth
                        explicitFocusMotorSteps
                        telescopeConfigs {
                          offsetMode
                          alongSlit { q { arcseconds } guiding }
                          toSky { offset { p { arcseconds } q { arcseconds } } guiding }
                        }
                        exposureTimeMode {
                          timeAndCount { time { seconds } count at { nanometers } }
                        }
                        acquisition {
                          explicitAcquisitionType
                          coadds
                          explicitFilter
                          skyOffset { p { arcseconds } q { arcseconds } }
                          exposureTimeMode {
                            signalToNoise { value at { nanometers } }
                            timeAndCount { time { seconds } count at { nanometers } }
                          }
                        }
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
                    "mode": "GNIRS_LONG_SLIT",
                    "gnirsSpectroscopy": {
                      "grating": "D111",
                      "initialGrating": "D111",
                      "prism": "MIRROR",
                      "initialPrism": "MIRROR",
                      "camera": "SHORT_BLUE",
                      "initialCamera": "SHORT_BLUE",
                      "fpuSlit": "LONG_SLIT_0_30",
                      "initialFpuSlit": "LONG_SLIT_0_30",
                      "filter": "ORDER3",
                      "initialFilter": "ORDER3",
                      "coadds": 1,
                      "decker": "SHORT_CAM_LONG_SLIT",
                      "defaultDecker": "SHORT_CAM_LONG_SLIT",
                      "explicitDecker": null,
                      "centralWavelength": { "nanometers": 2200.000 },
                      "initialCentralWavelength": { "nanometers": 2200.000 },
                      "explicitReadMode": null,
                      "wellDepth": "SHALLOW",
                      "defaultWellDepth": "SHALLOW",
                      "explicitWellDepth": null,
                      "explicitFocusMotorSteps": null,
                      "telescopeConfigs": {
                        "offsetMode": "NOD_ALONG_SLIT",
                        "alongSlit": [
                          { "q": { "arcseconds": 2.000000 },  "guiding": "ENABLED" },
                          { "q": { "arcseconds": -4.000000 }, "guiding": "ENABLED" },
                          { "q": { "arcseconds": -4.000000 }, "guiding": "ENABLED" },
                          { "q": { "arcseconds": 2.000000 },  "guiding": "ENABLED" }
                        ],
                        "toSky": null
                      },
                      "exposureTimeMode": {
                        "timeAndCount": {
                          "time": { "seconds": 30.000000 },
                          "count": 3,
                          "at": { "nanometers": 2200.000 }
                        }
                      },
                      "acquisition": {
                        "explicitAcquisitionType": null,
                        "coadds": 1,
                        "explicitFilter": null,
                        "skyOffset": null,
                        "exposureTimeMode": {
                          "signalToNoise": {
                            "value": 10.000,
                            "at": { "nanometers": 2200.000 }
                          },
                          "timeAndCount": null
                        }
                      }
                    }
                  }
                }
              }
            }
          """)
        )

  test("create GNIRS Long Slit with acquisition skyOffset — round-trips"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        exposureTimeMode: {
                          timeAndCount: {
                            time: { seconds: 30.0 }
                            count: 3
                            at: { nanometers: 2200 }
                          }
                        }
                        acquisition: {
                          explicitAcquisitionType: FAINT
                          skyOffset: {
                            p: { arcseconds: 1.5 }
                            q: { arcseconds: -2.5 }
                          }
                        }
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      gnirsSpectroscopy {
                        acquisition {
                          explicitAcquisitionType
                          skyOffset { p { arcseconds } q { arcseconds } }
                        }
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
                      "acquisition": {
                        "explicitAcquisitionType": "FAINT",
                        "skyOffset": {
                          "p": { "arcseconds": 1.500000 },
                          "q": { "arcseconds": -2.500000 }
                        }
                      }
                    }
                  }
                }
              }
            }
          """)
        )

  test("update GNIRS Long Slit — set acquisition skyOffset"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        for
          oid <- createGnirsLongSlitObservationAs(pi, pid, tid)
          _   <- expect(
            user  = pi,
            query =
              s"""
                mutation {
                  updateObservations(input: {
                    SET: {
                      observingMode: {
                        gnirsSpectroscopy: {
                          acquisition: {
                            explicitAcquisitionType: FAINT
                            skyOffset: {
                              p: { arcseconds: 3.0 }
                              q: { arcseconds: 4.0 }
                            }
                          }
                        }
                      }
                    }
                    WHERE: { id: { EQ: "$oid" } }
                  }) {
                    observations {
                      observingMode {
                        gnirsSpectroscopy {
                          acquisition {
                            explicitAcquisitionType
                            skyOffset { p { arcseconds } q { arcseconds } }
                          }
                        }
                      }
                    }
                  }
                }
              """,
            expected = Right(json"""
              {
                "updateObservations": {
                  "observations": [
                    {
                      "observingMode": {
                        "gnirsSpectroscopy": {
                          "acquisition": {
                            "explicitAcquisitionType": "FAINT",
                            "skyOffset": {
                              "p": { "arcseconds": 3.000000 },
                              "q": { "arcseconds": 4.000000 }
                            }
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """)
          )
        yield ()

  test("create GNIRS Long Slit with explicit overrides"):
    createProgramAs(staff).flatMap: pid =>
      createTargetAs(staff, pid).flatMap: tid =>
        expect(
          user  = staff,
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D32
                        prism: LXD
                        camera: LONG_RED
                        fpuSlit: LONG_SLIT_0_45
                        filter: ORDER3
                        centralWavelength: { nanometers: 2100 }
                        coadds: 2
                        explicitDecker: ACQUISITION
                        explicitReadMode: BRIGHT
                        explicitWellDepth: SHALLOW
                        explicitFocusMotorSteps: 500
                        exposureTimeMode: {
                          timeAndCount: {
                            time: { seconds: 10.0 }
                            count: 5
                            at: { nanometers: 2200 }
                          }
                        }
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      gnirsSpectroscopy {
                        grating
                        prism
                        camera
                        fpuSlit
                        filter
                        coadds
                        decker
                        defaultDecker
                        explicitDecker
                        centralWavelength { nanometers }
                        initialCentralWavelength { nanometers }
                        explicitReadMode
                        wellDepth
                        defaultWellDepth
                        explicitWellDepth
                        explicitFocusMotorSteps
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
                      "grating": "D32",
                      "prism": "LXD",
                      "camera": "LONG_RED",
                      "fpuSlit": "LONG_SLIT_0_45",
                      "filter": "ORDER3",
                      "coadds": 2,
                      "decker": "ACQUISITION",
                      "defaultDecker": "LONG_CAM_CROSS_DISPERSED",
                      "explicitDecker": "ACQUISITION",
                      "centralWavelength": { "nanometers": 2100.000 },
                      "initialCentralWavelength": { "nanometers": 2100.000 },
                      "explicitReadMode": "BRIGHT",
                      "wellDepth": "SHALLOW",
                      "defaultWellDepth": "DEEP",
                      "explicitWellDepth": "SHALLOW",
                      "explicitFocusMotorSteps": 500
                    }
                  }
                }
              }
            }
          """)
        )

  test("update GNIRS Long Slit — set explicit overrides"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        for
          oid <- createGnirsLongSlitObservationAs(pi, pid, tid)
          _   <- expect(
            user  = pi,
            query =
              s"""
                mutation {
                  updateObservations(input: {
                    SET: {
                      observingMode: {
                        gnirsSpectroscopy: {
                          explicitDecker: LONG_CAM_LONG_SLIT
                          explicitReadMode: VERY_FAINT
                          explicitWellDepth: DEEP
                        }
                      }
                    }
                    WHERE: { id: { EQ: "$oid" } }
                  }) {
                    observations {
                      observingMode {
                        gnirsSpectroscopy {
                          explicitDecker
                          explicitReadMode
                          explicitWellDepth
                        }
                      }
                    }
                  }
                }
              """,
            expected = Right(json"""
              {
                "updateObservations": {
                  "observations": [
                    {
                      "observingMode": {
                        "gnirsSpectroscopy": {
                          "explicitDecker": "LONG_CAM_LONG_SLIT",
                          "explicitReadMode": "VERY_FAINT",
                          "explicitWellDepth": "DEEP"
                        }
                      }
                    }
                  ]
                }
              }
            """)
          )
        yield ()

  test("PI cannot set explicitFocusMotorSteps on create — NotAuthorized"):
    interceptOdbError {
      createProgramAs(pi).flatMap: pid =>
        createTargetAs(pi, pid).flatMap: tid =>
          query(
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
                          focalPlane: SINGLE_SLIT
                          focalPlaneAngle: { microarcseconds: 0 }
                        }
                      }
                      observingMode: {
                        gnirsSpectroscopy: {
                          grating: D111
                          prism: MIRROR
                          camera: SHORT_BLUE
                          fpuSlit: LONG_SLIT_0_30
                          filter: ORDER3
                          centralWavelength: { nanometers: 2200 }
                          explicitFocusMotorSteps: 500
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
                  }) { observation { id } }
                }
              """
          )
    } {
      case OdbError.NotAuthorized(uid, _) if uid === pi.id => // expected
    }

  test("Staff can set explicitFocusMotorSteps on create"):
    createProgramAs(staff).flatMap: pid =>
      createTargetAs(staff, pid).flatMap: tid =>
        expect(
          user  = staff,
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        explicitFocusMotorSteps: 500
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
                      gnirsSpectroscopy { explicitFocusMotorSteps }
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
                    "gnirsSpectroscopy": { "explicitFocusMotorSteps": 500 }
                  }
                }
              }
            }
          """)
        )

  test("PI cannot set explicitFocusMotorSteps on update — NotAuthorized"):
    interceptOdbError {
      createProgramAs(pi).flatMap: pid =>
        createTargetAs(pi, pid).flatMap: tid =>
          for
            oid <- createGnirsLongSlitObservationAs(pi, pid, tid)
            _   <- query(
              user  = pi,
              query =
                s"""
                  mutation {
                    updateObservations(input: {
                      SET: {
                        observingMode: {
                          gnirsSpectroscopy: { explicitFocusMotorSteps: 500 }
                        }
                      }
                      WHERE: { id: { EQ: "$oid" } }
                    }) { observations { id } }
                  }
                """
            )
          yield ()
    } {
      case OdbError.NotAuthorized(uid, _) if uid === pi.id => // expected
    }

  test("PI can clear explicitFocusMotorSteps on update"):
    // PI owns the program; staff sets explicit focus; then PI clears it to null.
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createGnirsLongSlitObservationAs(pi, pid, tid)
      // Staff sets explicit focus
      _   <- query(
        user  = staff,
        query =
          s"""
            mutation {
              updateObservations(input: {
                SET: { observingMode: { gnirsSpectroscopy: { explicitFocusMotorSteps: 500 } } }
                WHERE: { id: { EQ: "$oid" } }
              }) { observations { id } }
            }
          """
      )
      _   <- expect(
        user  = pi,
        query =
          s"""
            mutation {
              updateObservations(input: {
                SET: { observingMode: { gnirsSpectroscopy: { explicitFocusMotorSteps: null } } }
                WHERE: { id: { EQ: "$oid" } }
              }) {
                observations {
                  observingMode {
                    gnirsSpectroscopy { explicitFocusMotorSteps }
                  }
                }
              }
            }
          """,
        expected = Right(json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "observingMode": {
                    "gnirsSpectroscopy": { "explicitFocusMotorSteps": null }
                  }
                }
              ]
            }
          }
        """)
      )
    yield ()

  test("Staff can set explicitFocusMotorSteps on update"):
    createProgramAs(staff).flatMap: pid =>
      createTargetAs(staff, pid).flatMap: tid =>
        for
          oid <- createGnirsLongSlitObservationAs(staff, pid, tid)
          _   <- expect(
            user  = staff,
            query =
              s"""
                mutation {
                  updateObservations(input: {
                    SET: {
                      observingMode: {
                        gnirsSpectroscopy: { explicitFocusMotorSteps: 500 }
                      }
                    }
                    WHERE: { id: { EQ: "$oid" } }
                  }) {
                    observations {
                      observingMode {
                        gnirsSpectroscopy { explicitFocusMotorSteps }
                      }
                    }
                  }
                }
              """,
            expected = Right(json"""
              {
                "updateObservations": {
                  "observations": [
                    {
                      "observingMode": {
                        "gnirsSpectroscopy": { "explicitFocusMotorSteps": 500 }
                      }
                    }
                  ]
                }
              }
            """)
          )
        yield ()

  test("clone GNIRS Long Slit observation preserves config"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        for
          oid  <- createGnirsLongSlitObservationAs(pi, pid, tid)
          oid2 <- cloneObservationAs(pi, oid)
          graph =
            """
            {
              observingMode {
                gnirsSpectroscopy {
                  grating
                  prism
                  camera
                  fpuSlit
                  filter
                }
              }
            }
            """
          _    <- expect(
            user  = pi,
            query =
              s"""
                query {
                  original: observation(observationId: "$oid") $graph
                  cloned:   observation(observationId: "$oid2") $graph
                }
              """,
            expected = Right(json"""
              {
                "original": {
                  "observingMode": {
                    "gnirsSpectroscopy": {
                      "grating": "D111",
                      "prism": "MIRROR",
                      "camera": "SHORT_BLUE",
                      "fpuSlit": "LONG_SLIT_0_30",
                      "filter": "ORDER3"
                    }
                  }
                },
                "cloned": {
                  "observingMode": {
                    "gnirsSpectroscopy": {
                      "grating": "D111",
                      "prism": "MIRROR",
                      "camera": "SHORT_BLUE",
                      "fpuSlit": "LONG_SLIT_0_30",
                      "filter": "ORDER3"
                    }
                  }
                }
              }
            """)
          )
        yield ()

  test("create GNIRS Long Slit with explicit acquisition filter"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        exposureTimeMode: {
                          timeAndCount: {
                            time: { seconds: 30.0 }
                            count: 3
                            at: { nanometers: 2200 }
                          }
                        }
                        acquisition: {
                          explicitFilter: H2
                        }
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      gnirsSpectroscopy {
                        acquisition { explicitFilter }
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
                      "acquisition": {
                        "explicitFilter": "H2"
                      }
                    }
                  }
                }
              }
            }
          """)
        )

  test("create GNIRS Long Slit rejects a non-acquisition explicit filter"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        exposureTimeMode: {
                          timeAndCount: {
                            time: { seconds: 30.0 }
                            count: 3
                            at: { nanometers: 2200 }
                          }
                        }
                        acquisition: {
                          explicitFilter: K
                        }
                      }
                    }
                  }
                }) { observation { id } }
              }
            """,
          expected = Left(List(
            "Argument 'input.SET.observingMode.gnirsSpectroscopy.acquisition' is invalid: 'explicitFilter' must contain one of: ORDER6, ORDER5, ORDER4, H2, ORDER3, PAH"
          ))
        )

  test("create GNIRS Long Slit rejects a sky offset without FAINT acquisition type"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        acquisition: {
                          explicitAcquisitionType: BRIGHT
                          skyOffset: { p: { arcseconds: 1.5 }, q: { arcseconds: -2.5 } }
                        }
                      }
                    }
                  }
                }) { observation { id } }
              }
            """,
          expected = Left(List(
            "Argument 'input.SET.observingMode.gnirsSpectroscopy.acquisition' is invalid: 'skyOffset' is only valid when 'explicitAcquisitionType' is FAINT."
          ))
        )

  test("create GNIRS Long Slit rejects FAINT acquisition type without a sky offset"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        acquisition: {
                          explicitAcquisitionType: FAINT
                        }
                      }
                    }
                  }
                }) { observation { id } }
              }
            """,
          expected = Left(List(
            "Argument 'input.SET.observingMode.gnirsSpectroscopy.acquisition' is invalid: 'explicitAcquisitionType' FAINT requires a 'skyOffset'."
          ))
        )

  test("create GNIRS Long Slit with explicit BRIGHT acquisition type and no sky offset"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        exposureTimeMode: {
                          timeAndCount: {
                            time: { seconds: 30.0 }
                            count: 3
                            at: { nanometers: 2200 }
                          }
                        }
                        acquisition: {
                          explicitAcquisitionType: BRIGHT
                        }
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      gnirsSpectroscopy {
                        acquisition {
                          explicitAcquisitionType
                          skyOffset { p { arcseconds } q { arcseconds } }
                        }
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
                      "acquisition": {
                        "explicitAcquisitionType": "BRIGHT",
                        "skyOffset": null
                      }
                    }
                  }
                }
              }
            }
          """)
        )

  test("create GNIRS Long Slit — telluricType defaults to HOT"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
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
                      gnirsSpectroscopy { telluricType { tag starTypes } }
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
                      "telluricType": { "tag": "HOT", "starTypes": null }
                    }
                  }
                }
              }
            }
          """)
        )

  test("create GNIRS Long Slit — explicit telluricType is stored"):
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
                        focalPlane: SINGLE_SLIT
                        focalPlaneAngle: { microarcseconds: 0 }
                      }
                    }
                    observingMode: {
                      gnirsSpectroscopy: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpuSlit: LONG_SLIT_0_30
                        filter: ORDER3
                        centralWavelength: { nanometers: 2200 }
                        telluricType: { tag: SOLAR }
                        exposureTimeMode: {
                          timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2200 } }
                        }
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      gnirsSpectroscopy { telluricType { tag starTypes } }
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
                      "telluricType": { "tag": "SOLAR", "starTypes": null }
                    }
                  }
                }
              }
            }
          """)
        )
