// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import io.circe.Json
import io.circe.literal.*

class observation_configuration extends OdbSuite with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val admin   = TestUsers.Standard.admin(2, 32)

  val validUsers = List(pi, admin).toList

  test("select configuration for fully-configured observation with sidereal target (gmos-n longslit)") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra {
                            hms
                          }
                          dec {
                            dms
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                          declinationArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                        gmosNorthImaging {
                          filters
                        }
                        gmosSouthImaging {
                          filters
                        }
                        flamingos2LongSlit {
                          disperser
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "observation" : {
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_ONE",
                        "cloudExtinction" : "POINT_ONE",
                        "skyBackground" : "DARKEST",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : {
                          "ra" : {
                            "hms" : "05:46:13.138550"
                          },
                          "dec" : {
                            "dms" : "-00:06:04.916774"
                          }
                        },
                        "region" : null
                      },
                      "observingMode" : {
                        "instrument" : "GMOS_NORTH",
                        "mode" : "GMOS_NORTH_LONG_SLIT",
                        "gmosNorthLongSlit" : {
                          "grating" : "R831_G5302"
                        },
                        "gmosSouthLongSlit" : null,
                        "gmosNorthImaging" : null,
                        "gmosSouthImaging" : null,
                        "flamingos2LongSlit" : null
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }

  test("select configuration for fully-configured observation with sidereal target (gmos-s longslit)") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosSouthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra {
                            hms
                          }
                          dec {
                            dms
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                          declinationArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                        gmosNorthImaging {
                          filters
                        }
                        gmosSouthImaging {
                          filters
                        }
                        flamingos2LongSlit {
                          disperser
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "observation" : {
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_ONE",
                        "cloudExtinction" : "POINT_ONE",
                        "skyBackground" : "DARKEST",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : {
                          "ra" : {
                            "hms" : "05:46:13.138550"
                          },
                          "dec" : {
                            "dms" : "-00:06:04.916774"
                          }
                        },
                        "region" : null
                      },
                      "observingMode" : {
                        "instrument" : "GMOS_SOUTH",
                        "mode" : "GMOS_SOUTH_LONG_SLIT",
                        "gmosNorthLongSlit" : null,
                        "gmosSouthLongSlit" : {
                          "grating" : "R600_G5324"
                        },
                        "gmosNorthImaging" : null,
                        "gmosSouthImaging" : null,
                        "flamingos2LongSlit" : null
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }

  test("select configuration for fully-configured observation with sidereal target (gmos-n imaging)") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosNorthImagingObservationAs(pi, pid, tid).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra {
                            hms
                          }
                          dec {
                            dms
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                          declinationArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                        gmosNorthImaging {
                          filters
                        }
                        gmosSouthImaging {
                          filters
                        }
                        flamingos2LongSlit {
                          disperser
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "observation" : {
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_EIGHT",
                        "cloudExtinction" : "POINT_THREE",
                        "skyBackground" : "BRIGHT",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : {
                          "ra" : {
                            "hms" : "05:46:13.138550"
                          },
                          "dec" : {
                            "dms" : "-00:06:04.916774"
                          }
                        },
                        "region" : null
                      },
                      "observingMode" : {
                        "instrument" : "GMOS_NORTH",
                        "mode" : "GMOS_NORTH_IMAGING",
                        "gmosNorthLongSlit" : null,
                        "gmosSouthLongSlit" : null,
                        "gmosNorthImaging" : {
                          "filters" : [
                            "G_PRIME",
                            "R_PRIME"
                          ]
                        },
                        "gmosSouthImaging" : null,
                        "flamingos2LongSlit" : null
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }

  test("select configuration for fully-configured observation with sidereal target (gmos-s imaging)") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosSouthImagingObservationAs(pi, pid, tid).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra {
                            hms
                          }
                          dec {
                            dms
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                          declinationArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                        gmosNorthImaging {
                          filters
                        }
                        gmosSouthImaging {
                          filters
                        }
                        flamingos2LongSlit {
                          disperser
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "observation" : {
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_EIGHT",
                        "cloudExtinction" : "POINT_THREE",
                        "skyBackground" : "BRIGHT",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : {
                          "ra" : {
                            "hms" : "05:46:13.138550"
                          },
                          "dec" : {
                            "dms" : "-00:06:04.916774"
                          }
                        },
                        "region" : null
                      },
                      "observingMode" : {
                        "instrument" : "GMOS_SOUTH",
                        "mode" : "GMOS_SOUTH_IMAGING",
                        "gmosNorthLongSlit" : null,
                        "gmosSouthLongSlit" : null,
                        "gmosNorthImaging" : null,
                        "gmosSouthImaging" : {
                          "filters" : [
                            "G_PRIME",
                            "R_PRIME"
                          ]
                        },
                        "flamingos2LongSlit" : null
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }

  test("select configuration for fully-configured observation with sidereal target (f2 logslit)") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createFlamingos2LongSlitObservationAs(pi, pid, tid).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra {
                            hms
                          }
                          dec {
                            dms
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                          declinationArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                        gmosNorthImaging {
                          filters
                        }
                        gmosSouthImaging {
                          filters
                        }
                        flamingos2LongSlit {
                          disperser
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "observation" : {
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_EIGHT",
                        "cloudExtinction" : "POINT_THREE",
                        "skyBackground" : "BRIGHT",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : {
                          "ra" : {
                            "hms" : "05:46:13.138550"
                          },
                          "dec" : {
                            "dms" : "-00:06:04.916774"
                          }
                        },
                        "region" : null
                      },
                      "observingMode" : {
                        "instrument" : "FLAMINGOS2",
                        "mode" : "FLAMINGOS_2_LONG_SLIT",
                        "gmosNorthLongSlit" : null,
                        "gmosSouthLongSlit" : null,
                        "gmosNorthImaging" : null,
                        "gmosSouthImaging" : null,
                        "flamingos2LongSlit" : {
                          "disperser" : "R1200_HK"
                        }
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }

  test("select configuration for non-fully-configured observation with sidereal target (no proposal)") {
    createCallForProposalsAs(admin).flatMap { _ =>
      createProgramAs(pi).flatMap { pid =>
        createTargetWithProfileAs(pi, pid).flatMap { tid =>
          createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra {
                            hms
                          }
                          dec {
                            dms
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                          declinationArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "observation" : {
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_ONE",
                        "cloudExtinction" : "POINT_ONE",
                        "skyBackground" : "DARKEST",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : null,
                        "region" : null
                      },
                      "observingMode" : {
                        "instrument" : "GMOS_NORTH",
                        "mode" : "GMOS_NORTH_LONG_SLIT",
                        "gmosNorthLongSlit" : {
                          "grating" : "R831_G5302"
                        },
                        "gmosSouthLongSlit" : null
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }

  test("select configuration for fully-configured observation with opportunity target") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createOpportunityTargetAs(pi, pid).flatMap { tid =>
          createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra {
                            hms
                          }
                          dec {
                            dms
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                          declinationArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "observation" : {
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_ONE",
                        "cloudExtinction" : "POINT_ONE",
                        "skyBackground" : "DARKEST",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : null,
                        "region" : {
                          "rightAscensionArc" : {
                            "type" : "FULL",
                            "start" : null,
                            "end" : null
                          },
                          "declinationArc" : {
                            "type" : "PARTIAL",
                            "start" : {
                              "degrees" : 10.0
                            },
                            "end" : {
                              "degrees" : 70.0
                            }
                          }
                        }
                      },
                      "observingMode" : {
                        "instrument" : "GMOS_NORTH",
                        "mode" : "GMOS_NORTH_LONG_SLIT",
                        "gmosNorthLongSlit" : {
                          "grating" : "R831_G5302"
                        },
                        "gmosSouthLongSlit" : null
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }


  test("select configuration for fully-configured observation with nonsidereal target (gmos-n longslit)") {
    createCallForProposalsAs(admin).flatMap { cfpid =>
      createProgramAs(pi, "Foo").flatMap { pid =>
        addProposal(pi, pid, Some(cfpid), None) >>
        createNonsiderealTargetAs(pi, pid).flatMap { tid =>
          createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).flatMap { oid =>
            expect(
              user = pi,
              query = s"""
                query {
                  observation(observationId: "$oid") {
                    configuration {
                      conditions {
                        imageQuality
                        cloudExtinction
                        skyBackground
                        waterVapor
                      }
                      target {
                        coordinates {
                          ra {
                            hms
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                          declinationArc {
                            type
                            start { degrees }
                            end { degrees }
                          }
                        }
                      }
                      observingMode {
                        instrument
                        mode
                        gmosNorthLongSlit {
                          grating
                        }
                        gmosSouthLongSlit {
                          grating
                        }
                        gmosNorthImaging {
                          filters
                        }
                        gmosSouthImaging {
                          filters
                        }
                        flamingos2LongSlit {
                          disperser
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(json"""
                {
                  "observation" : {
                    "configuration" : {
                      "conditions" : {
                        "imageQuality" : "POINT_ONE",
                        "cloudExtinction" : "POINT_ONE",
                        "skyBackground" : "DARKEST",
                        "waterVapor" : "WET"
                      },
                      "target" : {
                        "coordinates" : {
                          "ra" : {
                            "hms" : "08:09:46.797665"
                          }
                        },
                        "region" : null
                      },
                      "observingMode" : {
                        "instrument" : "GMOS_NORTH",
                        "mode" : "GMOS_NORTH_LONG_SLIT",
                        "gmosNorthLongSlit" : {
                          "grating" : "R831_G5302"
                        },
                        "gmosSouthLongSlit" : null,
                        "gmosNorthImaging" : null,
                        "gmosSouthImaging" : null,
                        "flamingos2LongSlit" : null
                      }
                    }
                  }
                }
              """)
            )
          }
        }
      }
    }
  }

}
