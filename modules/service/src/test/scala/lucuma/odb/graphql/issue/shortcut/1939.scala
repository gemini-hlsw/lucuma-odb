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
                    microarcseconds
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
                      "microseconds" : 12345,
                      "microarcseconds" : 185175
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

  test("RA microarcseconds should round-trip (when divisible by 15)") {
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
                    ra: { microarcseconds: 185175 }
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
                    microarcseconds
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
                      "microseconds" : 12345,
                      "microarcseconds" : 185175
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

  test("RA microarcseconds should round down (when not divisible by 15)") {
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
                    ra: { microarcseconds: 185183 }
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
                    microarcseconds
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
                      "microseconds" : 12345,
                      "microarcseconds" : 185175
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

