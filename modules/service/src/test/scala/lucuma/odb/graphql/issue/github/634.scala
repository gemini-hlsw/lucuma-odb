// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Target

// https://github.com/gemini-hlsw/lucuma-odb/issues/634
class GitHub_634 extends OdbSuite {
  val pi = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi)

  test("observation clone with new asterism") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).replicateA(3).map { 
        case List(t1, t2, t3) => (t1, t2, t3)
        case _                => fail("unpossible")
      }.flatMap { (t1, t2, t3) =>
        createObservationAs(pi, pid, t1, t2).flatMap { oid =>
          expect(
            user = pi,
            query = s"""
              mutation {
                cloneObservation(
                  input: {
                    observationId: ${oid.asJson}
                    SET: {
                      targetEnvironment: {
                          asterism: [ ${t3.asJson} ]
                      }
                    }
                  }
                ) {
                  originalObservation {
                    targetEnvironment {
                      asterism {
                        id
                      }
                    }
                  }
                  newObservation {
                    targetEnvironment {
                      asterism {
                        id
                      }
                    }
                  }
                }
              }
            """,
            expected = Right(json"""
              {
                "cloneObservation" : {
                  "originalObservation" : {
                    "targetEnvironment" : {
                      "asterism" : [
                        {
                          "id" : $t1
                        },
                        {
                          "id" : $t2
                        }
                      ]
                    }
                  },
                  "newObservation" : {
                    "targetEnvironment" : {
                      "asterism" : [
                        {
                          "id" : $t3
                        }
                      ]
                    }
                  }
                }
              }
            """) 
          )
        }
      }
    }
  }

}
