// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class exactConstraintsSuite extends GraphQLSuite:

  test("exactconditions: spectroscopy with exact imageQuality in arcsec"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              timeAndCount: {
                time: {
                  seconds: 2
                },
                count: 3,
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      }
                      brightnesses: [ {
                        band: R
                        value: 3
                        units: ERG_PER_S_PER_CM_SQUARED_PER_A
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
                arcsec: 0.85
              },
              cloudExtinction: {
                preset: POINT_FIVE
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
            exposureTimeMode {
              timeAndCount {
                time {
                  seconds
                }
                count
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy": {
              "mode": {
                "instrument": "GMOS_NORTH"
              },
              "exposureTimeMode": {
                "timeAndCount": {
                  "time": {
                    "seconds": 2.000000
                  },
                  "count": 3
                }
              }
            }
          }
        }
      """
    )

  test("exactconditions: spectroscopy with exact cloudExtinction magnitude"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 100
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  uniform: {
                    bandNormalized: {
                      sed: {
                        blackBodyTempK: 5000
                      }
                      brightnesses: [ {
                        band: R
                        value: 15
                        units: VEGA_MAG_PER_ARCSEC_SQUARED
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_THREE
              },
              cloudExtinction: {
                extinction: 0.3
              },
              skyBackground: BRIGHT,
              waterVapor: MEDIAN,
              elevationRange: {
                airMass: {
                  min: 1.2,
                  max: 1.8
                }
              }
            },
            mode: {
              gmosSSpectroscopy: {
                centralWavelength: {
                  nanometers: 700
                },
                filter: RG610,
                fpu: {
                  builtin: LONG_SLIT_0_50
                },
                grating: B1200_G5321
              }
            }
          }) {
            mode {
              ... on SpectroscopyMode {
                instrument
              }
            }
            exposureTimeMode {
              signalToNoise {
                value
                at {
                  nanometers
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy": {
              "mode": {
                "instrument": "GMOS_SOUTH"
              },
              "exposureTimeMode": {
                "signalToNoise": {
                  "value": 100,
                  "at": {
                    "nanometers": 600.000
                  }
                }
              }
            }
          }
        }
      """
    )

  test("exactconditions: imaging with both exact values"):
    query(
      """
        query {
          imaging(input: {
            exposureTimeMode: {
              timeAndCount: {
                time: {
                  seconds: 30
                },
                count: 5,
                at: { nanometers: 800 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  gaussian: {
                    fwhm: {
                      microarcseconds: 500000
                    }
                    spectralDefinition: {
                      bandNormalized: {
                        sed: {
                          powerLaw: 2.0
                        }
                        brightnesses: [ {
                          band: I
                          value: 18.5
                          units: AB_MAGNITUDE
                        }]
                      }
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 200
                }
              }
            ],
            constraints: {
              imageQuality: {
                arcsec: 1.2
              },
              cloudExtinction: {
                extinction: 0.15
              },
              skyBackground: GRAY,
              waterVapor: WET,
              elevationRange: {
                hourAngle: {
                  minHours: -4,
                  maxHours: 4
                }
              }
            },
            mode: {
              gmosNImaging: {
                filter: G_PRIME
              }
            }
          }) {
            mode {
              ... on ImagingMode {
                instrument
              }
            }
            exposureTimeMode {
              timeAndCount {
                time {
                  seconds
                }
                count
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "imaging": {
              "mode": {
                "instrument": "GMOS_NORTH"
              },
              "exposureTimeMode": {
                "timeAndCount": {
                  "time": {
                    "seconds": 30.000000
                  },
                  "count": 5
                }
              }
            }
          }
        }
      """
    )

  test("exactconditions: spectroscopy with variables using exact values"):
    query(
      """
        query($spectroscopy: SpectroscopyInput) {
          spectroscopy(input: $spectroscopy) {
            mode {
              ... on SpectroscopyMode {
                instrument
              }
            }
            exposureTimeMode {
              timeAndCount {
                time {
                  seconds
                }
                count
              }
            }
          }
        }
      """,
      """
        {
          "spectroscopy" : {
            "exposureTimeMode": {
              "timeAndCount": {
                "time": {
                  "seconds": 10
                },
                "count": 2,
                "at": { "nanometers": "500" }
              }
            },
            "asterism": [
              {
                "sourceProfile": {
                  "point": {
                    "bandNormalized": {
                      "sed": {
                        "stellarLibrary": "A0_V"
                      },
                      "brightnesses": [ {
                        "band": "V",
                        "value": "12.0",
                        "units": "VEGA_MAGNITUDE"
                      }]
                    }
                  }
                },
                "radialVelocity": {
                  "metersPerSecond": 5000
                }
              }
            ],
            "constraints" : {
              "imageQuality" : {
                "arcsec": "0.6"
              },
              "cloudExtinction" : {
                "extinction": "0.25"
              },
              "skyBackground" : "DARK",
              "waterVapor" : "DRY",
              "elevationRange" : {
                "airMass": {
                  "min": "1.0",
                  "max": "1.5"
                }
              }
            },
            "mode": {
              "flamingos2Spectroscopy": {
                "disperser": "R3000",
                "filter": "Y",
                "fpu": "LONG_SLIT_1"
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy": {
              "mode": {
                "instrument": "FLAMINGOS2"
              },
              "exposureTimeMode": {
                "timeAndCount": {
                  "time": {
                    "seconds": 10.000000
                  },
                  "count": 2
                }
              }
            }
          }
        }
      """
    )

  test("exactconditions: spectroscopy mixing preset and exact values"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 650 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: J
                        value: 16
                        units: AB_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: -50
                }
              }
            ],
            constraints: {
              imageQuality: {
                arcsec: 0.45
              },
              cloudExtinction: {
                preset: POINT_ONE
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 650
                },
                filter: GG455,
                fpu: {
                  builtin: LONG_SLIT_0_75
                },
                grating: R831_G5302
              }
            }
          }) {
            mode {
              ... on SpectroscopyMode {
                instrument
              }
            }
            exposureTimeMode {
              signalToNoise {
                value
                at {
                  nanometers
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy": {
              "mode": {
                "instrument": "GMOS_NORTH"
              },
              "exposureTimeMode": {
                "signalToNoise": {
                  "value": 50,
                  "at": {
                    "nanometers": 650.000
                  }
                }
              }
            }
          }
        }
      """
    )

  test("exactconditions: spectroscopy graphs with exact imageQuality"):
    query(
      """
        query {
          spectroscopyGraphs(input: {
            atWavelength: { nanometers: 500 },
            exposureTime: { seconds: 60 },
            exposureCount: 10,
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 14
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                arcsec: 0.7
              },
              cloudExtinction: {
                preset: POINT_THREE
              },
              skyBackground: GRAY,
              waterVapor: MEDIAN,
              elevationRange: {
                airMass: {
                  min: 1.1,
                  max: 1.6
                }
              }
            },
            mode: {
              gmosSSpectroscopy: {
                centralWavelength: {
                  nanometers: 500
                },
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_1_00
                },
                grating: B480_G5327
              }
            }
          }) {
            targetGraphs {
              ... on TargetGraphsResult {
                graphs {
                  peakFinalSNRatio
                  peakSingleSNRatio
                  ccds {
                    singleSNRatio
                    totalSNRatio
                  }
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopyGraphs": {
              "targetGraphs": [
                {
                  "graphs": {
                    "peakFinalSNRatio": 1009.000,
                    "peakSingleSNRatio": 1003.000,
                    "ccds": [
                      {
                        "singleSNRatio": 1.0,
                        "totalSNRatio": 2.0
                      }
                    ]
                  }
                }
              ]
            }
          }
        }
      """
    )

  test("exactconditions: cloudExtinction boundary minimum value"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_THREE
              },
              cloudExtinction: {
                extinction: 0.0
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy": {
              "mode": {
                "instrument": "GMOS_NORTH"
              }
            }
          }
        }
      """
    )

  test("exactconditions: cloudExtinction boundary maximum value"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_THREE
              },
              cloudExtinction: {
                extinction: 5.0
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy": {
              "mode": {
                "instrument": "GMOS_NORTH"
              }
            }
          }
        }
      """
    )

  test("exactconditions: imageQuality boundary minimum value"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                arcsec: 0.01
              },
              cloudExtinction: {
                preset: POINT_ONE
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy": {
              "mode": {
                "instrument": "GMOS_NORTH"
              }
            }
          }
        }
      """
    )

  test("exactconditions: imageQuality boundary maximum value"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                arcsec: 5.0
              },
              cloudExtinction: {
                preset: POINT_ONE
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopy": {
              "mode": {
                "instrument": "GMOS_NORTH"
              }
            }
          }
        }
      """
    )

  test("exactconditions: spectroscopy with both preset and exact imageQuality should fail"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_EIGHT,
                arcsec: 0.8
              },
              cloudExtinction: {
                preset: POINT_ONE
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
                },
                filter: GG455
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
              }
            }
          }
        }
      """,
      json"""
        {
          "errors": [
            {
              "message": "Argument 'input.constraints.imageQuality' is invalid: Expected exactly one of preset, arcsec",
              "extensions": {
                "odb_error" : {
                  "odb_error.tag" : "invalid_argument",
                  "odb_error.detail" : "Argument 'input.constraints.imageQuality' is invalid: Expected exactly one of preset, arcsec",
                  "odb_error.data" : {

                  }
                }
              }
            }
          ]
        }
      """
    )

  test("exactconditions: spectroscopy with both preset and exact cloudExtinction should fail"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_EIGHT
              },
              cloudExtinction: {
                preset: POINT_ONE,
                extinction: 0.1
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "errors": [
            {
              "message": "Argument 'input.constraints.cloudExtinction' is invalid: Expected exactly one of preset, extinction",
              "extensions": {
                "odb_error" : {
                  "odb_error.tag" : "invalid_argument",
                  "odb_error.detail" : "Argument 'input.constraints.cloudExtinction' is invalid: Expected exactly one of preset, extinction",
                  "odb_error.data" : {

                  }
                }
              }
            }
          ]
        }
      """
    )

  test("exactconditions: spectroscopy with out-of-range imageQuality should fail"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                arcsec: 6.0
              },
              cloudExtinction: {
                preset: POINT_ONE
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "errors": [
            {
              "message": "Invalid image quality value: 6.0"
            }
          ],
          "data": null
        }
      """
    )

  test("exactconditions: spectroscopy with out-of-range cloudExtinction should fail"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_EIGHT
              },
              cloudExtinction: {
                extinction: 6.0
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "errors": [
            {
              "message": "Invalid cloud extinction value: 6.0"
            }
          ],
          "data": null
        }
      """
    )

  test("exactconditions: spectroscopy with neither preset nor exact imageQuality should fail"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {},
              cloudExtinction: {
                preset: POINT_ONE
              },
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "errors": [
            {
              "message": "Argument 'input.constraints.imageQuality' is invalid: Expected exactly one of preset, arcsec",
              "extensions": {
                "odb_error" : {
                  "odb_error.tag" : "invalid_argument",
                  "odb_error.detail" : "Argument 'input.constraints.imageQuality' is invalid: Expected exactly one of preset, arcsec",
                  "odb_error.data" : {

                  }
                }
              }
            }
          ]
        }
      """
    )

  test("exactconditions: spectroscopy with neither preset nor exact cloudExtinction should fail"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 50
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G2_V
                      }
                      brightnesses: [ {
                        band: V
                        value: 12
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: POINT_EIGHT
              },
              cloudExtinction: {},
              skyBackground: DARK,
              waterVapor: DRY,
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
                  nanometers: 600
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
              }
            }
          }
        }
      """,
      json"""
        {
          "errors": [
            {
              "message": "Argument 'input.constraints.cloudExtinction' is invalid: Expected exactly one of preset, extinction",
              "extensions": {
                "odb_error" : {
                  "odb_error.tag" : "invalid_argument",
                  "odb_error.detail" : "Argument 'input.constraints.cloudExtinction' is invalid: Expected exactly one of preset, extinction",
                  "odb_error.data" : {

                  }
                }
              }
            }
          ]
        }
      """
    )
