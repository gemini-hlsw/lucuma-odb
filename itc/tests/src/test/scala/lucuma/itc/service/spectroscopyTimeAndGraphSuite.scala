// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class spectroscopyTimeAndGraphSuite extends GraphQLSuite {

  test("gmos graph") {
    query(
      """
        query {
          spectroscopyIntegrationTimeAndGraphs(input: {
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
                      }, {
                        band: J
                        value: 2.1
                        units: AB_MAGNITUDE
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
                preset: POINT_THREE
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
                exposureTimeMode: { signalToNoise: { value: 2, at: { nanometers: 60 } } },
                centralWavelength: {
                  nanometers: 60
                },
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            },
            significantFigures: {
              xAxis: 4
            }
          }) {
            targetTimesAndGraphs {
              ... on TargetTimeAndGraphs {
                integrationTime {
                  selected {
                    exposureTime {
                      seconds
                    }
                    exposureCount
                  }
                }
                graphs {
                  ccds {
                    singleSNRatio
                    totalSNRatio
                    peakPixelFlux
                    wellDepth
                    ampGain
                  }
                  graphData {
                    graphType
                    series {
                      title
                      seriesType
                      dataY
                      xAxis {
                        start
                        end
                        count
                      }
                      yAxis {
                        min
                        max
                       }
                    }
                  }
                }
              }
            }
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopyIntegrationTimeAndGraphs": {
              "targetTimesAndGraphs": [
                {
                  "integrationTime": {
                    "selected": {
                      "exposureCount" : 10,
                      "exposureTime" : {
                        "seconds" : 1.000000000
                      }
                    }
                  },
                  "graphs": {
                    "ccds" : [
                      {
                        "singleSNRatio" : 1.0,
                        "totalSNRatio" : 2.0,
                        "peakPixelFlux" : 3.0,
                        "wellDepth" : 4.0,
                        "ampGain" : 5.0
                      }
                    ],
                    "graphData": [
                      {
                        "graphType": "S2N_GRAPH",
                        "series": [
                          {
                            "title": "title",
                            "seriesType": "FINAL_S2_NDATA",
                            "dataY": [
                              1000.0,
                              1001.0
                            ],
                            "xAxis" : {
                              "start" : 1.0,
                              "end" : 2.0,
                              "count" : 2
                            },
                            "yAxis": {
                              "min": 1000.0,
                              "max": 1001.0
                            }
                          }
                        ]
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
  }

  test("igrins2 graph") {
    query(
      """
        query {
          spectroscopyIntegrationTimeAndGraphs(input: {
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        planet: JUPITER
                      }
                      brightnesses: [ {
                        band: R
                        value: 3
                        units: ERG_PER_S_PER_CM_SQUARED_PER_A
                      }, {
                        band: J
                        value: 2.1
                        units: AB_MAGNITUDE
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
                preset: POINT_THREE
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
              igrins2Spectroscopy: {
                exposureTimeMode: { signalToNoise: { value: 2, at: { nanometers: 1600 } } }
              }
            },
            significantFigures: {
              xAxis: 4
            }
          }) {
            targetTimesAndGraphs {
              ... on TargetTimeAndGraphs {
                integrationTime {
                  selected {
                    exposureTime {
                      seconds
                    }
                    exposureCount
                  }
                }
                graphs {
                  ccds {
                    singleSNRatio
                    totalSNRatio
                    peakPixelFlux
                    wellDepth
                    ampGain
                  }
                  graphData {
                    graphType
                    series {
                      title
                      seriesType
                      dataY
                      xAxis {
                        start
                        end
                        count
                      }
                      yAxis {
                        min
                        max
                      }
                    }
                  }
                }
              }
            }
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopyIntegrationTimeAndGraphs": {
              "targetTimesAndGraphs": [
                {
                  "integrationTime": {
                    "selected": {
                      "exposureCount" : 10,
                      "exposureTime" : {
                        "seconds" : 1.000000000
                      }
                    }
                  },
                  "graphs": {
                    "ccds" : [
                      {
                        "singleSNRatio" : 1.0,
                        "totalSNRatio" : 2.0,
                        "peakPixelFlux" : 3.0,
                        "wellDepth" : 4.0,
                        "ampGain" : 5.0
                      }
                    ],
                    "graphData": [
                      {
                        "graphType": "S2N_GRAPH",
                        "series": [
                          {
                            "title": "title",
                            "seriesType": "FINAL_S2_NDATA",
                            "dataY": [
                              1000.0,
                              1001.0
                            ],
                            "xAxis" : {
                              "start" : 1.0,
                              "end" : 2.0,
                              "count" : 2
                            },
                            "yAxis": {
                              "min": 1000.0,
                              "max": 1001.0
                            }
                          }
                        ]
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
  }

  test("gnirs graph") {
    query(
      """
        query {
          spectroscopyIntegrationTimeAndGraphs(input: {
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      }
                      brightnesses: [ {
                        band: J
                        value: 12
                        units: AB_MAGNITUDE
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
              gnirsSpectroscopy: {
                timeAndCount: { time: { seconds: 120 }, count: 30, at: { nanometers: 2200 } },
                centralWavelength: { nanometers: 2200 },
                filter: ORDER3,
                slitWidth: LONG_SLIT_0_30,
                prism: MIRROR,
                grating: D32,
                camera: SHORT_BLUE,
                readMode: BRIGHT,
                wellDepth: SHALLOW
              }
            },
            significantFigures: {
              xAxis: 4
            }
          }) {
            targetTimesAndGraphs {
              ... on TargetTimeAndGraphs {
                integrationTime {
                  selected {
                    exposureTime {
                      seconds
                    }
                    exposureCount
                  }
                }
                graphs {
                  ccds {
                    singleSNRatio
                    totalSNRatio
                    peakPixelFlux
                    wellDepth
                    ampGain
                  }
                  graphData {
                    graphType
                    series {
                      title
                      seriesType
                      data
                      xAxis {
                        start
                        end
                        count
                      }
                      dataY
                    }
                  }
                }
              }
            }
          }
        }
      """,
      json"""{
          "data": {
            "spectroscopyIntegrationTimeAndGraphs": {
              "targetTimesAndGraphs": [
                {
                  "integrationTime": {
                    "selected": {
                      "exposureCount" : 10,
                      "exposureTime" : {
                        "seconds" : 1.000000000
                      }
                    }
                  },
                  "graphs": {
                    "ccds" : [
                      {
                        "singleSNRatio" : 1.0,
                        "totalSNRatio" : 2.0,
                        "peakPixelFlux" : 3.0,
                        "wellDepth" : 4.0,
                        "ampGain" : 5.0
                      }
                    ],
                    "graphData": [
                      {
                        "graphType": "S2N_GRAPH",
                        "series": [
                          {
                            "title": "title",
                            "seriesType": "FINAL_S2_NDATA",
                            "data": [
                              [
                                1.0,
                                1000.0
                              ],
                              [
                                2.0,
                                1001.0
                              ]
                            ],
                            "xAxis" : {
                              "start" : 1.0,
                              "end" : 2.0,
                              "count" : 2
                            },
                            "dataY": [
                              1000.0,
                              1001.0
                            ]
                          }
                        ]
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
  }
}
