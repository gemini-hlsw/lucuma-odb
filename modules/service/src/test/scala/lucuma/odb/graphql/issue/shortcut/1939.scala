// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import io.circe.literal.*
import io.circe.syntax.*

// https://app.shortcut.com/lucuma/story/1939
class ShortCut_1939 extends OdbSuite {
  val pi = TestUsers.Standard.pi(nextId, nextId)
  lazy val validUsers = List(pi)

  test("RA microseconds should round-trip") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createTarget(
              input: {
                programId: ${pid.asJson}
                SET: {
                  name: "Crunchy Target"
                  sidereal: {
                    ra: { microseconds: 12345 }
                    dec: { degrees: 0 }
                    epoch: "J2000.000"
                  }
                  sourceProfile: {
                    point: {
                      bandNormalized: {
                        sed: { stellarLibrary: B5_III }
                        brightnesses: []
                      }
                    }
                  }
                }
              }
            ) {
              target {
                sidereal {
                  ra {
                    microseconds
                  }
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "createTarget" : {
                "target" : {
                  "sidereal" : {
                    "ra" : {
                      "microseconds" : 12345
                    }
                  }
                }
              }
            }
          """
        )
      )
    }
  }

}

