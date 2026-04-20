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
                exposureTimeMode: { signalToNoise: { value: 600, at: { picometers: 530000 } } },
                filter: G_PRIME
              }
            }
          }) {
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
                exposureTimeMode: { timeAndCount: { time: { seconds: 1.5 }, count: 5, at: { picometers: 530000 } } },
                filter: G_PRIME
              }
            }
          }) {
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
                exposureTimeMode: { signalToNoise: { value: 600, at: { picometers: 1650000 } } },
                filter: H,
                readMode: BRIGHT
              }
            }
          }) {
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
