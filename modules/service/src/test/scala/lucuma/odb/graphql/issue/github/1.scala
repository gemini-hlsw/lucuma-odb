// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import io.circe.Json
import io.circe.literal.*

// https://github.com/gemini-hlsw/lucuma-odb/issues/1
class GitHub_1 extends OdbSuite {

  val pi         = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  test("program users") {
    expect(
      user = pi,
      query = """
        mutation {
          createProgram(input:{ }) {
            program {
              pi { user { id } }
              users {
                user {
                  id
                }
              }
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "createProgram" : {
              "program" : {
                "pi" : {
                  "user": {
                    "id" : ${pi.id}
                  }
                },
                "users" : [
                ]
              }
            }
          }
        """
      )
    )
  }

}
