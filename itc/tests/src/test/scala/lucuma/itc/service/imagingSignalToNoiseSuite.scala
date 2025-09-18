// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class imagingSignalToNoiseSuite extends GraphImagingQLSuite:

  test("gmos north signal to noise"):
    query(
      """
        query {
          imaging(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 600,
                at: { picometers: 530000 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      },
                      brightnesses: [
                        {
                          band: SLOAN_I,
                          value: 9.484,
                          units: VEGA_MAGNITUDE,
                          error: 0.01
                        },
                        {
                          band: B,
                          value: 8.116,
                          units: VEGA_MAGNITUDE
                        },
                        {
                          band: V,
                          value: 12.323,
                          units: VEGA_MAGNITUDE,
                          error: 0.01
                        },
                        {
                          band: J,
                          value: 14.442,
                          units: VEGA_MAGNITUDE,
                          error: 0.018
                        },
                        {
                          band: H,
                          value: 9.798,
                          units: VEGA_MAGNITUDE,
                          error: 0.029
                        },
                        {
                          band: K,
                          value: 10.65,
                          units: VEGA_MAGNITUDE,
                          error: 0.03
                        }
                      ]
                    }
                  }
                },
                radialVelocity: {
                  metersPerSecond: 7560
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: TWO_POINT_ZERO
              },
              cloudExtinction: {
                preset: THREE_POINT_ZERO
              },
              skyBackground: BRIGHT,
              waterVapor: WET,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
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
              __typename
              ... on ImagingMode {
                instrument
                params {
                  ... on GmosNImagingParams {
                    filter
                  }
                }
              }
            }
            exposureTimeMode {
              signalToNoise {
                value
                at {
                  picometers
                }
              }
            }
            brightest {
              all {
                exposureCount
                exposureTime {
                  seconds
                }
              }
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "imaging" : {
              "mode" : {
                "__typename" : "ImagingMode",
                "instrument" : "GMOS_NORTH",
                "params": {
                  "filter": "G_PRIME"
                }
              },
              "exposureTimeMode": {
                "signalToNoise": {
                  "value": 600,
                  "at": { "picometers": 530000 }
                }
              },
              "brightest" : {
                "all" : [{
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                }, {
                  "exposureCount" : 5,
                  "exposureTime" : {
                    "seconds" : 2.000000
                  }
                }],
                "selected" : {
                  "exposureCount" : 5,
                  "exposureTime" : {
                    "seconds" : 2.000000
                  }
                }
              }
            }
          }
        }
        """
    )

  test("gmos north time and exposure"):
    query(
      """
        query {
          imaging(input: {
            exposureTimeMode: {
              timeAndCount: {
                time: { seconds: 1.5 },
                count: 5,
                at: { picometers: 530000 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      },
                      brightnesses: [
                        {
                          band: SLOAN_I,
                          value: 9.484,
                          units: VEGA_MAGNITUDE,
                          error: 0.01
                        },
                        {
                          band: B,
                          value: 8.116,
                          units: VEGA_MAGNITUDE
                        },
                        {
                          band: V,
                          value: 12.323,
                          units: VEGA_MAGNITUDE,
                          error: 0.01
                        },
                        {
                          band: J,
                          value: 14.442,
                          units: VEGA_MAGNITUDE,
                          error: 0.018
                        },
                        {
                          band: H,
                          value: 9.798,
                          units: VEGA_MAGNITUDE,
                          error: 0.029
                        },
                        {
                          band: K,
                          value: 10.65,
                          units: VEGA_MAGNITUDE,
                          error: 0.03
                        }
                      ]
                    }
                  }
                },
                radialVelocity: {
                  metersPerSecond: 7560
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: TWO_POINT_ZERO
              },
              cloudExtinction: {
                preset: THREE_POINT_ZERO
              },
              skyBackground: BRIGHT,
              waterVapor: WET,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
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
              __typename
              ... on ImagingMode {
                instrument
                params {
                  ... on GmosNImagingParams {
                    filter
                  }
                }
              }
            }
            exposureTimeMode {
              timeAndCount {
                time {
                  seconds
                }
                count
                at {
                  picometers
                }
              }
            }
            brightest {
              all {
                exposureCount
                exposureTime {
                  seconds
                }
              }
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "imaging" : {
              "mode" : {
                "__typename" : "ImagingMode",
                "instrument" : "GMOS_NORTH",
                "params": {
                  "filter": "G_PRIME"
                }
              },
              "exposureTimeMode": {
                "timeAndCount": {
                  "time": { "seconds": 1.5 },
                  "count": 5,
                  "at": { "picometers": 530000 }
                }
              },
              "brightest" : {
                "all" : [{
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                }],
                "selected" : {
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                }
              }
            }
          }
        }
        """
    )

  test("flamingos2 signal to noise"):
    query(
      """
        query {
          imaging(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 600,
                at: { picometers: 1650000 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      },
                      brightnesses: [
                        {
                          band: SLOAN_I,
                          value: 9.484,
                          units: VEGA_MAGNITUDE,
                          error: 0.01
                        },
                        {
                          band: B,
                          value: 8.116,
                          units: VEGA_MAGNITUDE
                        },
                        {
                          band: V,
                          value: 12.323,
                          units: VEGA_MAGNITUDE,
                          error: 0.01
                        },
                        {
                          band: J,
                          value: 14.442,
                          units: VEGA_MAGNITUDE,
                          error: 0.018
                        },
                        {
                          band: H,
                          value: 9.798,
                          units: VEGA_MAGNITUDE,
                          error: 0.029
                        },
                        {
                          band: K,
                          value: 10.65,
                          units: VEGA_MAGNITUDE,
                          error: 0.03
                        }
                      ]
                    }
                  }
                },
                radialVelocity: {
                  metersPerSecond: 7560
                }
              }
            ],
            constraints: {
              imageQuality: {
                preset: TWO_POINT_ZERO
              },
              cloudExtinction: {
                preset: THREE_POINT_ZERO
              },
              skyBackground: BRIGHT,
              waterVapor: WET,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              flamingos2Imaging: {
                filter: H
              }
            }
          }) {
            mode {
              __typename
              ... on ImagingMode {
                instrument
                params {
                  ... on Flamingos2ImagingParams {
                    filter
                  }
                }
              }
            }
            exposureTimeMode {
              signalToNoise {
                value
                at {
                  picometers
                }
              }
            }
            brightest {
              signalToNoiseAt {
                wavelength {
                  picometers
                }
                single
                total
              }
              all {
                exposureCount
                exposureTime {
                  seconds
                }
              }
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "imaging" : {
              "mode" : {
                "__typename" : "ImagingMode",
                "instrument" : "FLAMINGOS2",
                "params": {
                  "filter": "H"
                }
              },
              "exposureTimeMode": {
                "signalToNoise": {
                  "value": 600.000,
                  "at": { "picometers": 1650000 }
                }
              },
              "brightest" : {
                "signalToNoiseAt" : {
                  "wavelength" : {
                    "picometers" : 1650000
                  },
                  "single" : 101.000,
                  "total" : 102.000
                },
                "all" : [{
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                }, {
                  "exposureCount" : 5,
                  "exposureTime" : {
                    "seconds" : 2.000000
                  }
                }],
                "selected" : {
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                }
              }
            }
          }
        }
        """
    )
