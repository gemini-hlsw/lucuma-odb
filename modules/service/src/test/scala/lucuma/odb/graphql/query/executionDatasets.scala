// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.sequence.Dataset

class executionDatasets extends OdbSuite with ExecutionQuerySetupOperations with ExecutionTestSupportForGmos:

  val mode = ObservingModeType.GmosSouthLongSlit

  test("observation -> execution -> datasets") {
    recordAll(pi, serviceUser, mode, offset = 0, atomCount = 2, stepCount = 3, datasetCount = 2).flatMap { on =>
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
    recordAll(pi, serviceUser, mode, offset = 100, stepCount = 2, datasetCount = 2).flatMap { on =>
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

      val List(s0, s1) = on.visit.atoms.head.steps
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
    recordAll(pi, serviceUser, mode, offset = 200).flatMap { on =>
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

      val matches = on.allDatasets.map(_ => Json.obj("observation" -> Json.obj("id" -> on.id.asJson)))

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
    recordAll(pi, serviceUser, mode, offset = 300).flatMap { on =>
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

      val e = json"""
      {
        "observation": {
          "execution": {
            "datasets": {
              "matches": [
                {
                  "visit": {
                    "id": ${on.visit.id}
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