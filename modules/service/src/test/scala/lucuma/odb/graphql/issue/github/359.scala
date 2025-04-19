// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import io.circe.syntax.*

// https://github.com/gemini-hlsw/lucuma-odb/issues/359
class GitHub_359 extends OdbSuite {
  val pi = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi)

  test("selecting originalObservation's id and asterism (after clone) should work") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid).flatMap { oid =>
        query(
          user = pi,
          query = s"""
            mutation {
              cloneObservation(
                input: {
                  observationId: ${oid.asJson}
                }
              ) {
                originalObservation {
                  id
                  targetEnvironment {
                    asterism {
                      id
                    }
                  }
                }
              }
            }
          """
        )
      }
    }
  }

  test("selecting both observations' id and asterism (after clone) should work") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid).flatMap { oid =>
        query(
          user = pi,
          query = s"""
            mutation {
              cloneObservation(
                input: {
                  observationId: ${oid.asJson}
                }
              ) {
                originalObservation {
                  id
                  targetEnvironment {
                    asterism {
                      id
                    }
                  }
                }
                newObservation {
                  id
                  targetEnvironment {
                    asterism {
                      id
                    }
                  }
                }
              }
            }
          """
        )
      }
    }
  }

  test("selecting newObservation's id and asterism (after clone) should work") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid).flatMap { oid =>
        query(
          user = pi,
          query = s"""
            mutation {
              cloneObservation(
                input: {
                  observationId: ${oid.asJson}
                }
              ) {
                newObservation {
                  id
                  targetEnvironment {
                    asterism {
                      id
                    }
                  }
                }
              }
            }
          """
        )
      }
    }
  }

}
