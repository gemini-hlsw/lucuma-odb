// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset

class executionDatasets extends OdbSuite with ExecutionQuerySetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosSouthLongSlit

  val validUsers = List(pi, pi2, service).toList

  test("observation -> execution -> datasets") {
    recordAll(pi, service, mode, offset = 0, visitCount = 2, atomCount = 2, stepCount = 3, datasetCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              datasets() {
                matches {
                  id
                }
              }
            }
          }
        }
      """

      val matches = on.allDatasets.map(id => Json.obj("id" -> id.asJson))

      val e = json"""
      {
        "observation": {
          "execution": {
            "datasets": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> datasets -> events") {
    recordAll(pi, service, mode, offset = 100, stepCount = 2, datasetCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              datasets {
                matches {
                  events {
                    matches {
                      id
                    }
                  }
                }
              }
            }
          }
        }
      """

      val List(s0, s1) = on.visits.head.atoms.head.steps
      val matches      = (s0.datasets ++ s1.datasets).map { d =>
        Json.obj("events" -> Json.obj("matches" -> d.allEvents.map(e => Json.obj("id" -> e.id.asJson)).asJson))
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "datasets": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> datasets -> observation") {
    recordAll(pi, service, mode, offset = 200).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              datasets() {
                matches {
                  observation {
                    id
                  }
                }
              }
            }
          }
        }
      """

      val matches = on.allDatasets.map(id => Json.obj("observation" -> Json.obj("id" -> on.id.asJson)))

      val e = json"""
      {
        "observation": {
          "execution": {
            "datasets": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> datasets -> visit") {
    recordAll(pi, service, mode, offset = 300, visitCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              datasets() {
                matches {
                  visit {
                    id
                  }
                }
              }
            }
          }
        }
      """

      val List(v0, v1) = on.visits

      val e = json"""
      {
        "observation": {
          "execution": {
            "datasets": {
              "matches": [
                {
                  "visit": {
                    "id": ${v0.id}
                  }
                },
                {
                  "visit": {
                    "id": ${v1.id}
                  }
                }
              ]
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }
}
