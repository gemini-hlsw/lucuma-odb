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
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name" : "R150 1.5\""
            }
          ]
        }
      """.asRight
    )
  }
}
