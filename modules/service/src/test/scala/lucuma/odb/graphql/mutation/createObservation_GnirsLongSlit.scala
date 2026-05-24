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
                      gnirsLongSlit: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpu: LONG_SLIT_0_30
                        filter: ORDER3
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
                      gnirsLongSlit {
                        grating
                        initialGrating
                        prism
                        initialPrism
                        camera
                        initialCamera
                        fpu
                        initialFpu
                        filter
                        initialFilter
                        coadds
                        decker
                        defaultDecker
                        explicitDecker
                        gratingWavelength { nanometers }
                        defaultGratingWavelength { nanometers }
                        explicitGratingWavelength { nanometers }
                        readMode
                        defaultReadMode
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
                          readMode
                          coadds
                          filter
                          offset { p { arcseconds } q { arcseconds } }
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
                    "gnirsLongSlit": {
                      "grating": "D111",
                      "initialGrating": "D111",
                      "prism": "MIRROR",
                      "initialPrism": "MIRROR",
                      "camera": "SHORT_BLUE",
                      "initialCamera": "SHORT_BLUE",
                      "fpu": "LONG_SLIT_0_30",
                      "initialFpu": "LONG_SLIT_0_30",
                      "filter": "ORDER3",
                      "initialFilter": "ORDER3",
                      "coadds": 1,
                      "decker": "SHORT_CAM_LONG_SLIT",
                      "defaultDecker": "SHORT_CAM_LONG_SLIT",
                      "explicitDecker": null,
                      "gratingWavelength": { "nanometers": 2200.000 },
                      "defaultGratingWavelength": { "nanometers": 2200.000 },
                      "explicitGratingWavelength": null,
                      "readMode": "AUTOMATIC",
                      "defaultReadMode": "AUTOMATIC",
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
                        "readMode": "AUTOMATIC",
                        "coadds": 1,
                        "filter": "ORDER3",
                        "offset": null,
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
                      gnirsLongSlit: {
                        grating: D32
                        prism: LXD
                        camera: LONG_RED
                        fpu: LONG_SLIT_0_45
                        filter: ORDER3
                        coadds: 2
                        explicitDecker: ACQUISITION
                        explicitReadMode: BRIGHT
                        explicitWellDepth: SHALLOW
                        explicitGratingWavelength: { nanometers: 2100 }
                        explicitFocus: 500
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
                      gnirsLongSlit {
                        grating
                        prism
                        camera
                        fpu
                        filter
                        coadds
                        decker
                        defaultDecker
                        explicitDecker
                        gratingWavelength { nanometers }
                        defaultGratingWavelength { nanometers }
                        explicitGratingWavelength { nanometers }
                        readMode
                        defaultReadMode
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
                    "gnirsLongSlit": {
                      "grating": "D32",
                      "prism": "LXD",
                      "camera": "LONG_RED",
                      "fpu": "LONG_SLIT_0_45",
                      "filter": "ORDER3",
                      "coadds": 2,
                      "decker": "ACQUISITION",
                      "defaultDecker": "LONG_CAM_CROSS_DISPERSED",
                      "explicitDecker": "ACQUISITION",
                      "gratingWavelength": { "nanometers": 2100.000 },
                      "defaultGratingWavelength": { "nanometers": 2200.000 },
                      "explicitGratingWavelength": { "nanometers": 2100.000 },
                      "readMode": "BRIGHT",
                      "defaultReadMode": "AUTOMATIC",
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
                        gnirsLongSlit: {
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
                        gnirsLongSlit {
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
                        "gnirsLongSlit": {
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

  test("PI cannot set explicitFocus on create — NotAuthorized"):
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
                        gnirsLongSlit: {
                          grating: D111
                          prism: MIRROR
                          camera: SHORT_BLUE
                          fpu: LONG_SLIT_0_30
                          filter: ORDER3
                          explicitFocus: 500
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

  test("Staff can set explicitFocus on create"):
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
                      gnirsLongSlit: {
                        grating: D111
                        prism: MIRROR
                        camera: SHORT_BLUE
                        fpu: LONG_SLIT_0_30
                        filter: ORDER3
                        explicitFocus: 500
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
                      gnirsLongSlit { explicitFocusMotorSteps }
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
                    "gnirsLongSlit": { "explicitFocusMotorSteps": 500 }
                  }
                }
              }
            }
          """)
        )

  test("PI cannot set explicitFocus on update — NotAuthorized"):
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
                          gnirsLongSlit: { explicitFocus: 500 }
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

  test("PI cannot clear explicitFocus on update — NotAuthorized"):
    // First, staff creates and sets explicit focus; then PI tries to clear it.
    interceptOdbError {
      for
        pid <- createProgramAs(staff)
        tid <- createTargetAs(staff, pid)
        oid <- createGnirsLongSlitObservationAs(staff, pid, tid)
        // Staff sets explicit focus
        _   <- query(
          user  = staff,
          query =
            s"""
              mutation {
                updateObservations(input: {
                  SET: { observingMode: { gnirsLongSlit: { explicitFocus: 500 } } }
                  WHERE: { id: { EQ: "$oid" } }
                }) { observations { id } }
              }
            """
        )
        _   <- query(
          user  = pi,
          query =
            s"""
              mutation {
                updateObservations(input: {
                  SET: { observingMode: { gnirsLongSlit: { explicitFocus: null } } }
                  WHERE: { id: { EQ: "$oid" } }
                }) { observations { id } }
              }
            """
        )
      yield ()
    } {
      case OdbError.NotAuthorized(uid, _) if uid === pi.id => // expected
    }

  test("Staff can set explicitFocus on update"):
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
                        gnirsLongSlit: { explicitFocus: 500 }
                      }
                    }
                    WHERE: { id: { EQ: "$oid" } }
                  }) {
                    observations {
                      observingMode {
                        gnirsLongSlit { explicitFocusMotorSteps }
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
                        "gnirsLongSlit": { "explicitFocusMotorSteps": 500 }
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
                gnirsLongSlit {
                  grating
                  prism
                  camera
                  fpu
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
                    "gnirsLongSlit": {
                      "grating": "D111",
                      "prism": "MIRROR",
                      "camera": "SHORT_BLUE",
                      "fpu": "LONG_SLIT_0_30",
                      "filter": "ORDER3"
                    }
                  }
                },
                "cloned": {
                  "observingMode": {
                    "gnirsLongSlit": {
                      "grating": "D111",
                      "prism": "MIRROR",
                      "camera": "SHORT_BLUE",
                      "fpu": "LONG_SLIT_0_30",
                      "filter": "ORDER3"
                    }
                  }
                }
              }
            """)
          )
        yield ()
