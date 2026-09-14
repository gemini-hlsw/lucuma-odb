// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

// The Altair input is validated per mode: NGS needs every guide star field, LGS needs the guide
// star and always uses the field lens, LGS_P1 takes nothing.
class altairInputSuite extends GraphQLSuite:

  // Grackle prefixes a binding failure with the argument path
  private val Rejected: String = "Argument 'input.mode.gnirsSpectroscopy.altair' is invalid: "

  private def spectroscopyQuery(altair: String): String =
    s"""
      query {
        spectroscopy(input: {
          asterism: [
            {
              sourceProfile: {
                point: {
                  bandNormalized: {
                    sed: { stellarLibrary: O5_V }
                    brightnesses: [ { band: J, value: 12, units: AB_MAGNITUDE } ]
                  }
                }
              },
              radialVelocity: { kilometersPerSecond: 0 }
            }
          ],
          constraints: {
            imageQuality: { preset: POINT_THREE },
            cloudExtinction: { preset: POINT_FIVE },
            skyBackground: DARK,
            waterVapor: DRY,
            elevationRange: { airMass: { min: 1, max: 2 } }
          },
          mode: {
            gnirsSpectroscopy: {
              exposureTimeMode: { signalToNoise: { value: 100, at: { nanometers: 2200 } } },
              centralWavelength: { nanometers: 2200 },
              filter: ORDER3,
              fpu: { slitWidth: LONG_SLIT_0_30 },
              prism: MIRROR,
              grating: D32,
              camera: LONG_BLUE,
              readMode: BRIGHT,
              wellDepth: SHALLOW,
              coadds: 1,
              altair: $altair
            }
          }
        }) {
          targetTimes {
            ... on TargetIntegrationTime {
              band
            }
          }
        }
      }
    """

  test("NGS with every guide star field is accepted"):
    queryErrors(
      spectroscopyQuery(
        """{ mode: NGS, guideStarSeparation: { arcseconds: 3.5 }, guideStarBrightness: 12.5, fieldLens: OUT }"""
      ),
      Nil
    )

  test("NGS without the field lens is rejected"):
    queryErrors(
      spectroscopyQuery("""{ mode: NGS, guideStarSeparation: { arcseconds: 3.5 }, guideStarBrightness: 12.5 }"""),
      List(Rejected + "Altair NGS requires guideStarSeparation, guideStarBrightness and fieldLens.")
    )

  test("LGS with the guide star is accepted, with or without an explicit field lens in"):
    queryErrors(
      spectroscopyQuery("""{ mode: LGS, guideStarSeparation: { arcseconds: 3.5 }, guideStarBrightness: 15.5 }"""),
      Nil
    ) *>
      queryErrors(
        spectroscopyQuery(
          """{ mode: LGS, guideStarSeparation: { arcseconds: 3.5 }, guideStarBrightness: 15.5, fieldLens: IN }"""
        ),
        Nil
      )

  test("LGS with the field lens out is rejected"):
    queryErrors(
      spectroscopyQuery(
        """{ mode: LGS, guideStarSeparation: { arcseconds: 3.5 }, guideStarBrightness: 15.5, fieldLens: OUT }"""
      ),
      List(Rejected + "Altair LGS always uses the field lens; fieldLens must be IN or omitted.")
    )

  test("LGS without the guide star is rejected"):
    queryErrors(
      spectroscopyQuery("""{ mode: LGS }"""),
      List(Rejected + "Altair LGS requires guideStarSeparation and guideStarBrightness.")
    )

  test("LGS_P1 alone is accepted"):
    queryErrors(spectroscopyQuery("""{ mode: LGS_P1 }"""), Nil)

  test("LGS_P1 with guide star fields is rejected"):
    queryErrors(
      spectroscopyQuery("""{ mode: LGS_P1, guideStarBrightness: 12.5 }"""),
      List(Rejected + "Altair LGS_P1 takes no guide star parameters.")
    )

class altairImagingInputSuite extends GraphImagingQLSuite:

  test("GNIRS imaging accepts an Altair configuration"):
    queryErrors(
      """
      query {
        imaging(input: {
          asterism: [
            {
              sourceProfile: {
                point: {
                  bandNormalized: {
                    sed: { stellarLibrary: O5_V }
                    brightnesses: [ { band: K, value: 12, units: VEGA_MAGNITUDE } ]
                  }
                }
              },
              radialVelocity: { kilometersPerSecond: 0 }
            }
          ],
          constraints: {
            imageQuality: { preset: POINT_THREE },
            cloudExtinction: { preset: POINT_FIVE },
            skyBackground: DARK,
            waterVapor: DRY,
            elevationRange: { airMass: { min: 1, max: 2 } }
          },
          mode: {
            gnirsImaging: {
              exposureTimeMode: { signalToNoise: { value: 100, at: { nanometers: 2200 } } },
              filter: K,
              camera: LONG_BLUE,
              readMode: BRIGHT,
              wellDepth: SHALLOW,
              coadds: 1,
              altair: { mode: NGS, guideStarSeparation: { arcseconds: 0 }, guideStarBrightness: 9, fieldLens: IN }
            }
          }
        }) {
          brightest {
            band
          }
        }
      }
      """,
      Nil
    )
