// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                            exposureTimeMode: {
                              timeAndCount: {
                                time: { seconds: 30.0 }
                                count: 3
                                at: { nanometers: 2200 }
                              }
                            }
                          }
                        ]
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
                        filter
                        initialFilter
                        decker
                        defaultDecker
                        explicitDecker
                        centralWavelengths {
                          centralWavelength { nanometers }
                          coadds
                          exposureTimeMode {
                            timeAndCount { time { seconds } count at { nanometers } }
                          }
                        }
                        initialCentralWavelengths {
                          centralWavelength { nanometers }
                        }
                        explicitReadMode
                        wellDepth
                        defaultWellDepth
                        explicitWellDepth
                        explicitFocusMotorSteps
                        slit {
                          fpu
                          initialFpu
                          telescopeConfigs {
                            offsetMode
                            alongSlit { q { arcseconds } guiding }
                            toSky { offset { p { arcseconds } q { arcseconds } } guiding }
                          }
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
                      "filter": "ORDER3",
                      "initialFilter": "ORDER3",
                      "decker": "SHORT_CAM_LONG_SLIT",
                      "defaultDecker": "SHORT_CAM_LONG_SLIT",
                      "explicitDecker": null,
                      "centralWavelengths": [
                        {
                          "centralWavelength": { "nanometers": 2200.000 },
                          "coadds": 1,
                            "exposureTimeMode": {
                            "timeAndCount": {
                              "time": { "seconds": 30.000000 },
                              "count": 3,
                              "at": { "nanometers": 2200.000 }
                            }
                            }
                        }
                      ],
                      "initialCentralWavelengths": [
                        { "centralWavelength": { "nanometers": 2200.000 } }
                      ],
                      "explicitReadMode": null,
                      "wellDepth": "SHALLOW",
                      "defaultWellDepth": "SHALLOW",
                      "explicitWellDepth": null,
                      "explicitFocusMotorSteps": null,
                      "slit": {
                        "fpu": "LONG_SLIT_0_30",
                        "initialFpu": "LONG_SLIT_0_30",
                        "telescopeConfigs": {
                          "offsetMode": "NOD_ALONG_SLIT",
                          "alongSlit": [
                            { "q": { "arcseconds": 2.000000 },  "guiding": "ENABLED" },
                            { "q": { "arcseconds": -4.000000 }, "guiding": "ENABLED" },
                            { "q": { "arcseconds": -4.000000 }, "guiding": "ENABLED" },
                            { "q": { "arcseconds": 2.000000 },  "guiding": "ENABLED" }
                          ],
                          "toSky": null
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                            exposureTimeMode: {
                              timeAndCount: {
                                time: { seconds: 30.0 }
                                count: 3
                                at: { nanometers: 2200 }
                              }
                            }
                          }
                        ]
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
                        slit: { fpu: LONG_SLIT_0_45 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2100 }
                            coadds: 2
                            exposureTimeMode: {
                              timeAndCount: {
                                time: { seconds: 10.0 }
                                count: 5
                                at: { nanometers: 2200 }
                              }
                            }
                          }
                        ]
                        explicitDecker: ACQUISITION
                        explicitReadMode: BRIGHT
                        explicitWellDepth: SHALLOW
                        explicitFocusMotorSteps: 500
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
                        slit { fpu }
                        filter
                        decker
                        defaultDecker
                        explicitDecker
                        centralWavelengths {
                          centralWavelength { nanometers }
                          coadds
                          exposureTimeMode {
                            timeAndCount { time { seconds } count at { nanometers } }
                          }
                        }
                        initialCentralWavelengths {
                          centralWavelength { nanometers }
                        }
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
                      "slit": { "fpu": "LONG_SLIT_0_45" },
                      "filter": "ORDER3",
                      "decker": "ACQUISITION",
                      "defaultDecker": "LONG_CAM_CROSS_DISPERSED",
                      "explicitDecker": "ACQUISITION",
                      "centralWavelengths": [
                        {
                          "centralWavelength": { "nanometers": 2100.000 },
                          "coadds": 2,
                          "exposureTimeMode": {
                            "timeAndCount": {
                              "time": { "seconds": 10.000000 },
                              "count": 5,
                              "at": { "nanometers": 2200.000 }
                            }
                          }
                        }
                      ],
                      "initialCentralWavelengths": [
                        { "centralWavelength": { "nanometers": 2100.000 } }
                      ],
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
                          slit: { fpu: LONG_SLIT_0_30 }
                          filter: ORDER3
                          centralWavelengths: [
                            {
                              centralWavelength: { nanometers: 2200 }
                              exposureTimeMode: {
                                timeAndCount: {
                                  time: { seconds: 30.0 }
                                  count: 3
                                  at: { nanometers: 2200 }
                                }
                              }
                            }
                          ]
                          explicitFocusMotorSteps: 500
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                            exposureTimeMode: {
                              timeAndCount: {
                                time: { seconds: 30.0 }
                                count: 3
                                at: { nanometers: 2200 }
                              }
                            }
                          }
                        ]
                        explicitFocusMotorSteps: 500
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
                  slit { fpu }
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
                      "slit": { "fpu": "LONG_SLIT_0_30" },
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
                      "slit": { "fpu": "LONG_SLIT_0_30" },
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                            exposureTimeMode: {
                              timeAndCount: {
                                time: { seconds: 30.0 }
                                count: 3
                                at: { nanometers: 2200 }
                              }
                            }
                          }
                        ]
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                            exposureTimeMode: {
                              timeAndCount: {
                                time: { seconds: 30.0 }
                                count: 3
                                at: { nanometers: 2200 }
                              }
                            }
                          }
                        ]
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                          }
                        ]
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                          }
                        ]
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                            exposureTimeMode: {
                              timeAndCount: {
                                time: { seconds: 30.0 }
                                count: 3
                                at: { nanometers: 2200 }
                              }
                            }
                          }
                        ]
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                            exposureTimeMode: {
                              timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2200 } }
                            }
                          }
                        ]
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
                        slit: { fpu: LONG_SLIT_0_30 }
                        filter: ORDER3
                        centralWavelengths: [
                          {
                            centralWavelength: { nanometers: 2200 }
                            exposureTimeMode: {
                              timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2200 } }
                            }
                          }
                        ]
                        telluricType: { tag: SOLAR }
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

  // The central wavelength list is ordered by the user, not by wavelength: the order given
  // is the order the sequence executes the wavelengths in, and the first entry is the one
  // acquisition is sized for.  A wavelength may also repeat, each occurrence being an
  // independent configuration.
  private def createWithWavelengths(pid: String, tid: String, wavelengths: String, selection: String): String =
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
                slit: { fpu: LONG_SLIT_0_30 }
                filter: ORDER3
                centralWavelengths: $wavelengths
              }
            }
          }
        }) {
          observation {
            observingMode {
              gnirsSpectroscopy { $selection }
            }
          }
        }
      }
    """

  private def timeAndCount(nm: Int, count: Int): String =
    s"""
      {
        centralWavelength: { nanometers: $nm }
        exposureTimeMode: {
          timeAndCount: { time: { seconds: 30.0 } count: $count at: { nanometers: $nm } }
        }
      }
    """

  test("create GNIRS Long Slit preserves the central wavelength order as given"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query = createWithWavelengths(
            pid.toString,
            tid.toString,
            s"[ ${timeAndCount(2300, 3)} ${timeAndCount(2100, 4)} ${timeAndCount(2200, 5)} ]",
            """
              centralWavelengths        { centralWavelength { nanometers } }
              initialCentralWavelengths { centralWavelength { nanometers } }
            """
          ),
          // Descending-then-ascending: neither sorted order nor reverse-sorted order, so a
          // reintroduced sort anywhere in the round trip shows up here.
          expected = Right(json"""
            {
              "createObservation": {
                "observation": {
                  "observingMode": {
                    "gnirsSpectroscopy": {
                      "centralWavelengths": [
                        { "centralWavelength": { "nanometers": 2300.000 } },
                        { "centralWavelength": { "nanometers": 2100.000 } },
                        { "centralWavelength": { "nanometers": 2200.000 } }
                      ],
                      "initialCentralWavelengths": [
                        { "centralWavelength": { "nanometers": 2300.000 } },
                        { "centralWavelength": { "nanometers": 2100.000 } },
                        { "centralWavelength": { "nanometers": 2200.000 } }
                      ]
                    }
                  }
                }
              }
            }
          """)
        )

  test("create GNIRS Long Slit accepts a repeated central wavelength with its own exposure time mode"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query = createWithWavelengths(
            pid.toString,
            tid.toString,
            s"""[
              {
                centralWavelength: { nanometers: 2200 }
                exposureTimeMode: {
                  timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2200 } }
                }
                coadds: 2
              }
              {
                centralWavelength: { nanometers: 2200 }
                exposureTimeMode: {
                  timeAndCount: { time: { seconds: 60.0 } count: 7 at: { nanometers: 2200 } }
                }
                coadds: 4
              }
            ]""",
            """
              centralWavelengths {
                centralWavelength { nanometers }
                coadds
                exposureTimeMode { timeAndCount { time { seconds } count } }
              }
            """
          ),
          expected = Right(json"""
            {
              "createObservation": {
                "observation": {
                  "observingMode": {
                    "gnirsSpectroscopy": {
                      "centralWavelengths": [
                        {
                          "centralWavelength": { "nanometers": 2200.000 },
                          "coadds": 2,
                          "exposureTimeMode": {
                            "timeAndCount": { "time": { "seconds": 30.000000 }, "count": 3 }
                          }
                        },
                        {
                          "centralWavelength": { "nanometers": 2200.000 },
                          "coadds": 4,
                          "exposureTimeMode": {
                            "timeAndCount": { "time": { "seconds": 60.000000 }, "count": 7 }
                          }
                        }
                      ]
                    }
                  }
                }
              }
            }
          """)
        )

  // Identical entries are the case that breaks if the exposure time mode resolution key
  // does not carry the list index: nothing else tells the two rows apart.
  test("create GNIRS Long Slit accepts two identical central wavelength entries"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query = createWithWavelengths(
            pid.toString,
            tid.toString,
            s"[ ${timeAndCount(2200, 3)} ${timeAndCount(2200, 3)} ]",
            """
              centralWavelengths {
                centralWavelength { nanometers }
                exposureTimeMode { timeAndCount { count } }
              }
            """
          ),
          expected = Right(json"""
            {
              "createObservation": {
                "observation": {
                  "observingMode": {
                    "gnirsSpectroscopy": {
                      "centralWavelengths": [
                        {
                          "centralWavelength": { "nanometers": 2200.000 },
                          "exposureTimeMode": { "timeAndCount": { "count": 3 } }
                        },
                        {
                          "centralWavelength": { "nanometers": 2200.000 },
                          "exposureTimeMode": { "timeAndCount": { "count": 3 } }
                        }
                      ]
                    }
                  }
                }
              }
            }
          """)
        )

  // The cap and the empty check are the two ends of the same validation, and the upper
  // one also pins the Int -> Short narrowing behind `c_index`: the last accepted entry
  // sits at index 99, which must survive the round trip through the smallint column and
  // come back last.
  private def wavelengthList(n: Int): String =
    (0 until n).map(i => timeAndCount(1000 + i, 3)).mkString("[ ", " ", " ]")

  test("create GNIRS Long Slit accepts the maximum central wavelength list"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        query(
          user  = pi,
          query = createWithWavelengths(
            pid.toString,
            tid.toString,
            wavelengthList(100),
            "centralWavelengths { centralWavelength { nanometers } }"
          )
        ).map: js =>
          val ws =
            js.hcursor
              .downFields("createObservation", "observation", "observingMode", "gnirsSpectroscopy", "centralWavelengths")
              .values
              .toList
              .flatten
              .flatMap(_.hcursor.downFields("centralWavelength", "nanometers").as[BigDecimal].toOption)
              .map(_.toInt)
          assertEquals(ws, (0 until 100).map(1000 + _).toList)

  test("create GNIRS Long Slit rejects a central wavelength list over the maximum"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query = createWithWavelengths(pid.toString, tid.toString, wavelengthList(101), "centralWavelengths { coadds }"),
          expected = Left(List(
            "Argument 'input.SET.observingMode.gnirsSpectroscopy' is invalid: At most 100 central wavelengths may be specified for GNIRS spectroscopy observations."
          ))
        )

  test("create GNIRS Long Slit rejects an empty central wavelength list"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query = createWithWavelengths(pid.toString, tid.toString, "[]", "centralWavelengths { coadds }"),
          expected = Left(List(
            "Argument 'input.SET.observingMode.gnirsSpectroscopy' is invalid: At least one central wavelength must be specified for GNIRS spectroscopy observations."
          ))
        )
