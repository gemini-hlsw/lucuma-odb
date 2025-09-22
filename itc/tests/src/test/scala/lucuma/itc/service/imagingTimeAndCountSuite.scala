// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class imagingTimeAndCountSuite extends GraphImagingQLSuite:

  test("flamingos2 time and count"):
    query(
      """
        query {
          imaging(input: {
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
              timeAndCount {
                time {
                  seconds
                }
                count
                at {
                  nanometers
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
              ccds {
                singleSNRatio
                totalSNRatio
                peakPixelFlux
                warnings {
                  msg
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
                "timeAndCount": {
                  "time": {
                    "seconds": 2
                  },
                  "count": 3,
                  "at": { "nanometers": 600.000 }
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
                },
                "ccds" : [
                  {
                    "singleSNRatio" : 50.5,
                    "totalSNRatio" : 150.5,
                    "peakPixelFlux" : 35000.0,
                    "warnings" : [
                      {
                        "msg" : "Saturation. Warning: Peak pixel intensity exceeds 80% of the pixel full well depth; Peak pixel flux = 100.000%"
                      }
                    ]
                  },
                  {
                    "singleSNRatio" : 45.2,
                    "totalSNRatio" : 135.7,
                    "peakPixelFlux" : 28000.0,
                    "warnings" : []
                  }
                ]
              }
            }
          }
        }
        """
    )
