// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*

class spectroscopyConfigOptions extends OdbSuite {

  val pi = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  test("simple query") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              focalPlane: { EQ: SINGLE_SLIT }
              resolution: { GTE: 7000 }
            }
          ) {
            name
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name": "B1200 0.25\""
            },
            {
              "name": "R831 0.25\""
            }
          ]
        }
      """.asRight
    )
  }

  test("""331" < SlitWidth """) {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              slitLength: { microarcseconds: { GTE: 331000000 } }
            }
          ) {
            name
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
          ]
        }
      """.asRight
    )
  }

  test("""1" < SlitWidth < 2"""") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              slitWidth: {
                AND: [
                  { arcseconds: { GT: 1.0 } },
                  { arcseconds: { LT: 2.0 } }
                ]
              }
            }
          ) {
            name
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name" : "B600 1.5\""
            },
            {
              "name" : "R831 1.5\""
            },
            {
              "name" : "B480 1.5\""
            },
            {
              "name" : "R400 1.5\""
            },
            {
              "name" : "R150 1.5\""
            },
            {
              "name" : "B1200 1.5\""
            }
          ]
        }
      """.asRight
    )
  }

  test("Combined") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              capability: { IS_NULL: true }
              focalPlane: { EQ: SINGLE_SLIT }
              instrument: { EQ: GMOS_NORTH }
              resolution: { LT: 500 }
              site: { EQ: GN }
              slitWidth: {
                AND: [
                  { arcseconds: { GT: 1.0 } },
                  { arcseconds: { LT: 2.0 } }
                ]
              }
            }
          ) {
            name
            instrument

            focalPlane

            fpuLabel
            slitWidth { arcseconds }
            slitLength { arcseconds }

            disperserLabel
            filterLabel

            wavelengthMin { micrometers }
            wavelengthMax { micrometers }
            wavelengthOptimal { micrometers }
            wavelengthCoverage { micrometers }

            resolution
            adaptiveOptics
            capability
            site

          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name" : "R150 1.5\"",
              "instrument": "GMOS_NORTH",
              "focalPlane" : "SINGLE_SLIT",
              "fpuLabel" : "1.5\"",
              "slitWidth" : {
                "arcseconds" : 1.5
              },
              "slitLength" : {
                "arcseconds" : 330
              },
              "disperserLabel" : "R150",
              "filterLabel": null,
              "wavelengthMin" : {
                "micrometers" : 0.360000
              },
              "wavelengthMax" : {
                "micrometers" : 1.030000
              },
              "wavelengthOptimal" : {
                "micrometers" : 0.717000
              },
              "wavelengthCoverage" : {
                "micrometers" : 1.219000
              },
              "resolution" : 210,
              "adaptiveOptics" : false,
              "capability" : null,
              "site" : "GN"
            }
          ]
        }
      """.asRight
    )
  }

  test("GmosNorth") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              focalPlane: { EQ: SINGLE_SLIT }
              instrument: { EQ: GMOS_NORTH }
              resolution: { LT: 500 }
              slitWidth: {
                AND: [
                  { arcseconds: { GT: 1.0 } },
                  { arcseconds: { LT: 2.0 } }
                ]
              }
            }
          ) {
            name
            gmosNorth {
              fpu
              grating
            }
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name" : "R150 1.5\"",
              "gmosNorth": {
                "fpu": "LONG_SLIT_1_50",
                "grating": "R150_G5308"
              }
            }
          ]
        }
      """.asRight
    )
  }
}
