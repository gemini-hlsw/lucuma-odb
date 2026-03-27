// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class WavelengthAtOutOfRangeErrorSuite extends WavelengthAtOutOfRangeSuite:

  test("igrins2 spectroscopy at 1800nm returns wavelength_at_out_of_range error"):
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
                exposureTimeMode: { signalToNoise: { value: 2, at: { nanometers: 1800 } } }
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
            }
          }
        }
        """,
      json"""
        {
          "errors": [{
            "message": "The requested wavelength falls outside the instrument's wavelength coverage.",
            "extensions": {
              "targetIndex": 0,
              "error": {
                "wellHalfFilledSeconds": null,
                "wavelength": {
                  "picometers": 1800000,
                  "angstroms": 18000.0,
                  "nanometers": 1800.000,
                  "micrometers": 1.800000
                },
                "errorCode": "WAVELENGTH_AT_OUT_OF_RANGE",
                "message": "The requested wavelength falls outside the instrument's wavelength coverage."
              }
            }
          }],
          "data": {
            "spectroscopy" : {
              "brightest" : null
            }
          }
        }
        """
    )
