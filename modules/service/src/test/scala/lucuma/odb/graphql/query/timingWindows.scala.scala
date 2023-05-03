// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import io.circe.syntax._
import lucuma.odb.graphql.OdbSuite

class timingWindows extends OdbSuite {
  val pi       = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  test("we should be able to query constraints and timing windows") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        expect(
            user = user,
            query =
              s"""
              query {
                observations(programId: ${pid.asJson}) {
                  matches {
                    constraintSet {
                      elevationRange {
                        airMass {
                          min
                        }
                      }
                    }
                    timingWindows {
                      end {
                        ... on TimingWindowEndAfter {
                          duration {
                            milliseconds
                          }         
                        }
                      }
                    }
                  }
                }
              }
              """,
            expected = Right(
              json"""
                {
                  "observations" : {
                    "matches" : [
                    ]
                  }
                }
              """
            )
        )
      }
    }
  }
}
