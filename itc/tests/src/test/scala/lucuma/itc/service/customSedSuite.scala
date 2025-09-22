// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class customSedSuite extends GraphQLSuite {

  test("custom sed in request") {
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 2,
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        fluxDensities: [
                          { wavelength: { nanometers: 500.0 }, density: 1.0 },
                          { wavelength: { nanometers: 600.0 }, density: 2.0 },
                          { wavelength: { nanometers: 700.0 }, density: 3.0 }
                        ]
                      }
                      brightnesses: [{
                        band: R
                        value: 9
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 1000
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_EIGHT
              },
              cloudExtinction: {
                preset: ONE_POINT_ZERO
              },
              skyBackground: BRIGHT,
              waterVapor: MEDIAN,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
                centralWavelength: {
                  nanometers: 60
                },
                filter: GG455,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
            mode {
              ... on SpectroscopyMode {
                instrument
                params {
                  ... on GmosNSpectroscopyParams {
                    grating
                    centralWavelength {
                      nanometers
                    }
                  }
                }
              }
            }
            brightest {
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
              band
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy" : {
              "mode" : {
                "instrument" : "GMOS_NORTH",
                "params": {
                  "grating": "B1200_G5301",
                  "centralWavelength" : {
                    "nanometers" : 60.000
                  }
                }
              },
              "brightest": {
                "selected" : {
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                },
                "band": "R"
              }
            }
          }
        }
        """
    )
  }

  test("custom sed in file") {
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 2,
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        fluxDensitiesAttachment: "a-1"
                      }
                      brightnesses: [{
                        band: R
                        value: 9
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 1000
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_EIGHT
              },
              cloudExtinction: {
                preset: ONE_POINT_ZERO
              },
              skyBackground: BRIGHT,
              waterVapor: MEDIAN,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
                centralWavelength: {
                  nanometers: 60
                },
                filter: GG455,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
            mode {
              ... on SpectroscopyMode {
                instrument
                params {
                  ... on GmosNSpectroscopyParams {
                    grating
                    centralWavelength {
                      nanometers
                    }
                  }
                }
              }
            }
            brightest {
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
              band
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy" : {
              "mode" : {
                "instrument" : "GMOS_NORTH",
                "params": {
                  "grating": "B1200_G5301",
                  "centralWavelength" : {
                    "nanometers" : 60.000
                  }
                }
              },
              "brightest": {
                "selected" : {
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                },
                "band": "R"
              }
            }
          }
        }
        """
    )
  }

  test("custom sed in empty file") {
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 2,
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        fluxDensitiesAttachment: "a-999"
                      }
                      brightnesses: [{
                        band: R
                        value: 9
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 1000
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_EIGHT
              },
              cloudExtinction: {
                preset: ONE_POINT_ZERO
              },
              skyBackground: BRIGHT,
              waterVapor: MEDIAN,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
                centralWavelength: {
                  nanometers: 60
                },
                filter: GG455,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
            mode {
              ... on SpectroscopyMode {
                instrument
                params {
                  ... on GmosNSpectroscopyParams {
                    grating
                    centralWavelength {
                      nanometers
                    }
                  }
                }
              }
            }
            brightest {
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
              band
            }
          }
        }
      """,
      json"""
        {
          "errors": [
            {
              "message" : "Error calculating ITC: Custom SED file for id [a-999] is empty.",
              "extensions" : {
                "targetIndex" : 0,
                "error" : {
                  "wellHalfFilledSeconds" : null,
                  "errorCode" : "GENERAL",
                  "message" : "Error calculating ITC: Custom SED file for id [a-999] is empty."
                }
              }
            }
          ],
          "data": {
            "spectroscopy" : {
              "mode" : {
                "instrument" : "GMOS_NORTH",
                "params" : {
                  "grating" : "B1200_G5301",
                  "centralWavelength" : {
                    "nanometers" : 60.000
                  }
                }
              },
              "brightest" : null
            }
          }
        }
        """
    )
  }

  test("custom sed in invalid file") {
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 2,
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        fluxDensitiesAttachment: "a-2"
                      }
                      brightnesses: [{
                        band: R
                        value: 9
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 1000
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_EIGHT
              },
              cloudExtinction: {
                preset: ONE_POINT_ZERO
              },
              skyBackground: BRIGHT,
              waterVapor: MEDIAN,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
                centralWavelength: {
                  nanometers: 60
                },
                filter: GG455,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
            mode {
              ... on SpectroscopyMode {
                instrument
                params {
                  ... on GmosNSpectroscopyParams {
                    grating
                    centralWavelength {
                      nanometers
                    }
                  }
                }
              }
            }
            brightest {
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
              band
            }
          }
        }
      """,
      json"""
        {
          "errors": [
            {
              "message" : "Error calculating ITC: Invalid wavelength in custom SED: [someText].Invalid density in custom SED: [someOtherText].",
              "extensions" : {
                "targetIndex" : 0,
                "error" : {
                  "wellHalfFilledSeconds" : null,
                  "errorCode" : "GENERAL",
                  "message" : "Error calculating ITC: Invalid wavelength in custom SED: [someText].Invalid density in custom SED: [someOtherText]."
                }
              }
            }
          ],
          "data": {
            "spectroscopy" : {
              "mode" : {
                "instrument" : "GMOS_NORTH",
                "params" : {
                  "grating" : "B1200_G5301",
                  "centralWavelength" : {
                    "nanometers" : 60.000
                  }
                }
              },
              "brightest" : null
            }
          }
        }
        """
    )
  }
}
