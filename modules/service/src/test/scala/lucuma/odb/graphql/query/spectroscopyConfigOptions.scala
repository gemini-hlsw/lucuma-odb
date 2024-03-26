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
            requirements: {
              focalPlane: SINGLE_SLIT
              resolution: 7000
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
              "name": "B1200 0.25"
            },
            {
              "name": "R831 0.25"
            }
          ]
        }
      """.asRight
    )
  }
}
