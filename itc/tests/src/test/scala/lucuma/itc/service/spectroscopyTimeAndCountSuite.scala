// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.*
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated

class spectroscopyTimeAndCountSuite extends GraphQLSuite:

  test("gmos north case"):
    query(
      """
        query {
          spectroscopy(input: {
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
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 600 } } },
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
            targetTimes {
              ... on TargetIntegrationTime {
                selected {
                  exposureCount
                  exposureTime {
                    seconds
                  }
                }
                index
                all {
                  exposureCount
                  exposureTime {
                    seconds
                  }
                }
                signalToNoiseAt {
                  wavelength {
                    nanometers
                  }
                  single
                  total
                }
              }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "spectroscopy" : {
              "targetTimes": [
                {
                  "selected": {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2
                    }
                  },
                  "index": 0,
                  "all": [
                    {
                      "exposureCount": 10,
                      "exposureTime": {
                        "seconds": 2
                      }
                    }
                  ],
                  "signalToNoiseAt": {
                    "wavelength": {
                      "nanometers": 600.000
                    },
                    "single": 101.000,
                    "total": 102.000
                  }
                }
              ]
            }
          }
        }
        """
    )

  test("gmos south case"):
    query(
      """
        query {
          spectroscopy(input: {
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
                        error: 0.2
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
              gmosSSpectroscopy: {
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                centralWavelength: {
                  nanometers: 60
                },
                filter: RG610,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5321
              }
            }
          }) {
              targetTimes {
                ... on TargetIntegrationTime {
                  selected {
                    exposureCount
                    exposureTime {
                      seconds
                    }
                  }
                  index
                  signalToNoiseAt {
                    wavelength {
                      nanometers
                    }
                    single
                    total
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
            "spectroscopy" : {
                "targetTimes": [
                  {
                    "selected": {
                      "exposureCount": 10,
                      "exposureTime": {
                        "seconds": 2
                      }
                    },
                    "index": 0,
                    "signalToNoiseAt": {
                      "wavelength": {
                        "nanometers": 60.000
                      },
                      "single": 101.000,
                      "total": 102.000
                    }
                  }
                ],
                "brightest": {
                  "selected": {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2
                    }
                  },
                  "ccds": []
                }
              }
            }
        }
        """
    )

  test("gmos north case with variables"):
    query(
      """
        query($spectroscopy: SpectroscopyInput) {\n          spectroscopy(input: $spectroscopy) {\n            brightestIndex }\n            }\n
      """,
      """
        {
          "spectroscopy" : {
            "asterism": [
              {
                "sourceProfile": {
                  "uniform": {
                    "bandNormalized": {
                      "sed": {
                        "stellarLibrary": "O5_V"
                      },
                      "brightnesses": [ {
                        "band": "R",
                        "value": "3",
                        "units": "VEGA_MAG_PER_ARCSEC_SQUARED"
                      }, {
                        "band": "K",
                        "value": "2.1",
                        "units": "W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED"
                      }]
                    }
                  }
                },
                "radialVelocity": {
                  "metersPerSecond": 1000
                }
              }
            ],
            "constraints" : {
              "imageQuality" : {
                "preset": "POINT_EIGHT"
              },
              "cloudExtinction" : {
                "preset": "POINT_FIVE"
              },
              "skyBackground" : "DARK",
              "waterVapor" : "DRY",
              "elevationRange" : {
                "airMass": {
                  "min": "1.1",
                  "max": "1.3"
                }
              }
            },
            "mode": {
              "gmosNSpectroscopy": {
                "exposureTimeMode": { "timeAndCount": { "time": { "seconds": 2 }, "count": 3, "at": { "nanometers": "600" } } },
                "centralWavelength": {
                  "nanometers": "600"
                },
                "filter": "G_PRIME",
                "fpu": {
                  "builtin": "LONG_SLIT_0_25"
                },
                "grating": "B1200_G5301"
              }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "spectroscopy" :
              {
                "brightestIndex" : 0
              }
          }
        }
        """
    )

  val allConditions =
    for {
      iq <- Enumerated[ImageQuality.Preset].all
      ce <- Enumerated[CloudExtinction.Preset].all
      wv <- Enumerated[WaterVapor].all
      sb <- Enumerated[SkyBackground].all
    } yield ItcObservingConditions(iq.toImageQuality.toArcSeconds,
                                   ce.toCloudExtinction.toVegaMagnitude,
                                   wv,
                                   sb,
                                   2
    )

  val conditions = ItcObservingConditions(
    ImageQuality.Preset.PointEight.toImageQuality.toArcSeconds,
    CloudExtinction.Preset.OnePointZero.toCloudExtinction.toVegaMagnitude,
    WaterVapor.Median,
    SkyBackground.Bright,
    2
  )

  test("iterate over conditions"):
    allConditions.traverse { c =>
      query(
        s"""
        query {
          spectroscopy(input: {
            asterism: [
              {
                sourceProfile: {
                  uniform: {
                    bandNormalized: {
                      sed: {
                        powerLaw: 3.0
                      },
                      brightnesses: [ {
                        band: R,
                        value: 3,
                        units: VEGA_MAG_PER_ARCSEC_SQUARED
                      }, {
                        band: K,
                        value: 2.1,
                        units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                      }]
                    }
                  }
                },
                radialVelocity: {
                  centimetersPerSecond: 1000
                }
              }
            ],
            constraints: {
              imageQuality: {
                arcsec: ${c.iq}
              },
              cloudExtinction: {
                extinction: ${c.cc}
              },
              skyBackground: ${c.sb.tag.toScreamingSnakeCase},
              waterVapor: ${c.wv.tag.toScreamingSnakeCase},
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                centralWavelength: {
                  nanometers: 60
                },
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
                targetTimes {
                  ... on TargetIntegrationTime {
                    signalToNoiseAt {
                      wavelength {
                        nanometers
                      }
                      single
                      total
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
            "spectroscopy" :
              {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "wavelength": {
                        "nanometers": 60.000
                      },
                      "single": 101.000,
                      "total": 102.000
                    }
                  }
                ],
                "brightest": {
                  "selected": {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
      )
    }

  test("Bad airmass"):
    query(
      """
        query {
          spectroscopy(input: {
            asterism: [
              {
                sourceProfile: {
                  uniform: {
                    bandNormalized: {
                      sed: {
                        blackBodyTempK: 100
                      },
                      brightnesses: [ {
                        band: R,
                        value: 3,
                        units: VEGA_MAG_PER_ARCSEC_SQUARED
                      }, {
                        band: K,
                        value: 2.1,
                        units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                      }]
                    }
                  }
                },
                radialVelocity: {
                  metersPerSecond: 1000
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
                  min: 2,
                  max: 1
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 600 } } },
                centralWavelength: {
                  nanometers: 60
                },
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
              brightest {
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
          "data": null,
          "errors": [
            {
              "message" : "Creating an air mass range requires specifying both min and max where min < max"
            }
          ]
        }
        """
    )

  test("gmosN_gratings"):
    Enumerated[GmosNorthGrating].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopy(input: {
            asterism: [
              {
                sourceProfile: {
                  uniform: {
                    bandNormalized: {
                      sed: {
                        blackBodyTempK: 100
                      },
                      brightnesses: [ {
                        band: R,
                        value: 3,
                        units: VEGA_MAG_PER_ARCSEC_SQUARED
                      }, {
                        band: K,
                        value: 2.1,
                        units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                      }]
                    }
                  }
                },
                radialVelocity: {
                  centimetersPerSecond: 1000
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
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                centralWavelength: {
                  nanometers: 60
                },
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: ${d.tag.toScreamingSnakeCase}
              }
            }
          }) {
                targetTimes {
                  ... on TargetIntegrationTime {
                    signalToNoiseAt {
                      wavelength {
                        nanometers
                      }
                      single
                      total
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
            "spectroscopy" :
              {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "wavelength": {
                        "nanometers": 60.000
                      },
                      "single": 101.000,
                      "total": 102.000
                    }
                  }
                ],
                "brightest": {
                  "selected": {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
      )
    }

  test("gmosS_gratings"):
    Enumerated[GmosSouthGrating].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopy(input: {
            asterism: [
              {
                sourceProfile: {
                  gaussian: {
                    fwhm: {
                      microarcseconds: 100
                    }
                    spectralDefinition: {
                      bandNormalized: {
                        sed: {
                          blackBodyTempK: 100
                        },
                        brightnesses: [ {
                          band: R,
                          value: 3,
                          units: VEGA_MAGNITUDE
                        }, {
                          band: K,
                          value: 2.1,
                          units: JANSKY
                        }]
                      }
                    }
                  }
                },
                radialVelocity: {
                  centimetersPerSecond: 1000
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
              gmosSSpectroscopy: {
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                centralWavelength: {
                  nanometers: 60
                },
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: ${d.tag.toScreamingSnakeCase}
              }
            }
          }) {
                targetTimes {
                  ... on TargetIntegrationTime {
                    signalToNoiseAt {
                      wavelength {
                        nanometers
                      }
                      single
                      total
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
            "spectroscopy" :
              {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "wavelength": {
                        "nanometers": 60.000
                      },
                      "single": 101.000,
                      "total": 102.000
                    }
                  }
                ],
                "brightest": {
                  "selected": {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
      )
    }

  test("gmosN_fpu"):
    Enumerated[GmosNorthFpu].all.traverse { d =>
      query(
        s"""
          query {
            spectroscopy(input: {
              asterism: [
                {
                  sourceProfile: {
                    gaussian: {
                      fwhm: {
                        microarcseconds: 100
                      }
                      spectralDefinition: {
                        bandNormalized: {
                          sed: {
                            blackBodyTempK: 100
                          },
                          brightnesses: [ {
                            band: R,
                            value: 3,
                            units: VEGA_MAGNITUDE
                          }, {
                            band: K,
                            value: 2.1,
                            units: JANSKY
                          }]
                        }
                      }
                    }
                  },
                  radialVelocity: {
                    centimetersPerSecond: 1000
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
                  exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                  centralWavelength: {
                    nanometers: 60,
                  },
                  filter: G_PRIME,
                  fpu: {
                    builtin: ${d.tag.toScreamingSnakeCase}
                  },
                  grating: B1200_G5301
                }
              }
            }) {
                  targetTimes {
                    ... on TargetIntegrationTime {
                      signalToNoiseAt {
                        wavelength {
                          nanometers
                        }
                        single
                        total
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
            "spectroscopy" :
              {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "wavelength": {
                        "nanometers": 60.000
                      },
                      "single": 101.000,
                      "total": 102.000
                    }
                  }
                ],
                "brightest": {
                  "selected": {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
      )
    }

  test("gmosS_fpu"):
    Enumerated[GmosSouthFpu].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopy(input: {
            asterism: [
              {
                sourceProfile: {
                  uniform: {
                    bandNormalized: {
                      sed: {
                        blackBodyTempK: 100
                      },
                      brightnesses: [ {
                        band: R,
                        value: 3,
                        units: VEGA_MAG_PER_ARCSEC_SQUARED
                      }, {
                        band: K,
                        value: 2.1,
                        units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                      } ]
                    }
                  }
                },
                radialVelocity: {
                  centimetersPerSecond: 1000
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
              gmosSSpectroscopy: {
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                centralWavelength: {
                  nanometers: 60,
                },
                filter: G_PRIME,
                fpu: {
                  builtin: ${d.tag.toScreamingSnakeCase}
                },
                grating: B1200_G5321
              }
            }
          }) {
                targetTimes {
                  ... on TargetIntegrationTime {
                    signalToNoiseAt {
                      wavelength {
                        nanometers
                      }
                      single
                      total
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
            "spectroscopy" :
              {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "wavelength": {
                        "nanometers": 60.000
                      },
                      "single": 101.000,
                      "total": 102.000
                    }
                  }
                ],
                "brightest": {
                  "selected": {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
      )
    }

  test("gmosN_filter"):
    Enumerated[GmosNorthFilter].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopy(input: {
            asterism: [
              {
                sourceProfile: {
                  uniform: {
                    bandNormalized: {
                      sed: {
                        blackBodyTempK: 100
                      },
                      brightnesses: [ {
                        band: R,
                        value: 3,
                        units: VEGA_MAG_PER_ARCSEC_SQUARED
                      }, {
                        band: K,
                        value: 2.1,
                        units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                      }]
                    }
                  }
                },
                radialVelocity: {
                  centimetersPerSecond: 1000
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
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                centralWavelength: {
                  nanometers: 60,
                },
                filter: ${d.tag.toScreamingSnakeCase}
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
                targetTimes {
                  ...on TargetIntegrationTime {
                    signalToNoiseAt {
                      wavelength {
                        nanometers
                      }
                      single
                      total
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
            "spectroscopy" :
              {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "wavelength": {
                        "nanometers": 60.000
                      },
                      "single": 101.000,
                      "total": 102.000
                    }
                  }
                ],
                "brightest": {
                  "selected": {
                      "exposureCount": 10,
                      "exposureTime": {
                        "seconds": 2
                      }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
      )
    }

  test("gmosS_filter"):
    Enumerated[GmosSouthFilter].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopy(input: {
            asterism: [
              {
                sourceProfile: {
                  uniform: {
                    bandNormalized: {
                      sed: {
                        blackBodyTempK: 100
                      },
                      brightnesses: [ {
                        band: R,
                        value: 3,
                        units: VEGA_MAG_PER_ARCSEC_SQUARED
                      }, {
                        band: K,
                        value: 2.1,
                        units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                      }]
                    }
                  }
                },
                radialVelocity: {
                  centimetersPerSecond: 1000
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
              gmosSSpectroscopy: {
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                centralWavelength: {
                  nanometers: 60,
                },
                filter: ${d.tag.toScreamingSnakeCase}
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5321
              }
            }
          }) {
                targetTimes {
                  ...on TargetIntegrationTime {
                    selected {
                      exposureCount
                      exposureTime {
                        seconds
                      }
                    }
                    signalToNoiseAt {
                      wavelength {
                        nanometers
                      }
                      single
                      total
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
            "spectroscopy" :
              {
                "targetTimes": [
                  {
                    "selected": {
                      "exposureCount": 10,
                      "exposureTime": {
                        "seconds": 2
                      }
                    },
                    "signalToNoiseAt": {
                      "wavelength": {
                        "nanometers": 60.000
                      },
                      "single": 101.000,
                      "total": 102.000
                    }
                  }
                ],
                "brightest": {
                  "selected": {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
      )
    }

  test("multiple targets"):
    query(
      """
        query {
          spectroscopy(input: {
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
              },
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      }
                      brightnesses: [ {
                        band: R
                        value: 5.1
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
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
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
            targetTimes {
              ...on TargetIntegrationTime {
                signalToNoiseAt {
                  wavelength {
                    nanometers
                  }
                  single
                  total
                }
                selected {
                  exposureCount
                  exposureTime {
                    seconds
                  }
                }
                band
              }
            }
            brightestIndex
            brightest {
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
            "spectroscopy" : {
              "targetTimes": [
                {
                  "signalToNoiseAt": {
                    "wavelength": {
                      "nanometers": 60.000
                    },
                    "single": 101.000,
                    "total": 102.000
                  },
                  "selected" : {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2.000000
                    }
                  },
                  "band": "R"
                },
                {
                  "signalToNoiseAt": {
                    "wavelength": {
                      "nanometers": 60.000
                    },
                    "single": 101.000,
                    "total": 102.000
                  },
                  "selected" : {
                    "exposureCount": 10,
                    "exposureTime": {
                      "seconds": 2.000000
                    }
                  },
                  "band": "R"
                }
              ],
              "brightestIndex": 0,
              "brightest": {
                "selected": {
                  "exposureCount": 10,
                  "exposureTime": {
                    "seconds": 2.000000
                  }
                },
                "ccds": []
              }
            }
          }
        }
        """
    )

  test("f2 case") {
    query(
      """
        query {
          spectroscopy(input: {
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
                        error: 0.2
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
              flamingos2Spectroscopy: {
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 60 } } },
                filter: Y,
                fpu: LONG_SLIT_1,
                disperser: R3000
              }
            }
          }) {
              targetTimes {
                ... on TargetIntegrationTime {
                  signalToNoiseAt {
                    single
                    total
                    wavelength {
                      nanometers
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
            "spectroscopy" : {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "single": 101.000000,
                      "total": 102.000000,
                      "wavelength": {
                        "nanometers": 60.000
                      }
                    }
                  }
                ],
                "brightest": {
                  "selected" : {
                    "exposureCount" : 10,
                    "exposureTime" : {
                      "seconds" : 2.000000
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
    )
  }

  test("igrins2 case"):
    query(
      """
        query {
          spectroscopy(input: {
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
                        error: 0.2
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
                exposureTimeMode: { timeAndCount: { time: { seconds: 2 }, count: 3, at: { nanometers: 1600 } } }
              }
            }
          }) {
              targetTimes {
                ... on TargetIntegrationTime {
                  signalToNoiseAt {
                    single
                    total
                    wavelength {
                      nanometers
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
            "spectroscopy" : {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "single": 101.000000,
                      "total": 102.000000,
                      "wavelength": {
                        "nanometers": 1600.000
                      }
                    }
                  }
                ],
                "brightest": {
                  "selected" : {
                    "exposureCount" : 10,
                    "exposureTime" : {
                      "seconds" : 2.000000
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
    )

  test("ghost case"):
    query(
      """
        query {
          spectroscopy(input: {
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
                        error: 0.2
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
              ghostSpectroscopy: {
                centralWavelength: {
                  nanometers: 1600
                },
                numSkyMicrolens: 7,
                resolutionMode: HIGH,
                redDetector: {
                  timeAndCount: {
                    time: {
                      seconds: 2
                    },
                    count: 3,
                    at: {
                      nanometers: 1600
                    }
                  },
                  readMode: SLOW,
                  binning: ONE_BY_ONE
                },
                blueDetector: {
                  timeAndCount: {
                    time: {
                      seconds: 2                    },
                    count: 3,
                    at: {
                      nanometers: 1600
                    }
                  },
                  readMode: SLOW,
                  binning: ONE_BY_ONE
                }
              }
            }
          }) {
              targetTimes {
                ... on TargetIntegrationTime {
                  signalToNoiseAt {
                    single
                    total
                    wavelength {
                      nanometers
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
            "spectroscopy" : {
                "targetTimes": [
                  {
                    "signalToNoiseAt": {
                      "single": 101.000000,
                      "total": 102.000000,
                      "wavelength": {
                        "nanometers": 1600.000
                      }
                    }
                  }
                ],
                "brightest": {
                  "selected" : {
                    "exposureCount" : 10,
                    "exposureTime" : {
                      "seconds" : 2.000000
                    }
                  },
                  "ccds": []
                }
              }
          }
        }
        """
    )
