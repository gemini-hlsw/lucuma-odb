// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.data.ObservingModeType

class executionAtomRecords extends OdbSuite with ExecutionQuerySetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

  /*
  test("observation -> execution -> datasets") {
    recordAll(mode, Setup(offset = 0, visitCount = 2, atomCount = 2, stepCount = 3, datasetCount = 2), pi).flatMap { on =>
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
    recordAll(mode, Setup(offset = 100, stepCount = 2, datasetCount = 2), pi).flatMap { on =>
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
        Json.obj("events" -> Json.obj("matches" -> d.allEvents.map(eid => Json.obj("id" -> eid.asJson)).asJson))
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
    recordAll(mode, Setup(offset = 200), pi).flatMap { on =>
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
    recordAll(mode, Setup(offset = 300, visitCount = 2), pi).flatMap { on =>
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

  test("observation -> execution -> events") {
    recordAll(mode, Setup(offset = 400, visitCount = 2, atomCount = 2, stepCount = 3, datasetCount = 2), pi).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              events() {
                matches {
                  id
                }
              }
            }
          }
        }
      """

      val events = on.allEvents.map(id => Json.obj("id" -> id.asJson))

      val e = json"""
      {
        "observation": {
          "execution": {
            "events": {
              "matches": $events
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> visits") {
    recordAll(mode, Setup(offset = 500, visitCount = 2), pi).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              visits() {
                matches {
                  id
                }
              }
            }
          }
        }
      """

      val visits = on.visits.map(_.id).map(id => Json.obj("id" -> id.asJson))

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": $visits
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> visits -> datasets") {
    recordAll(mode, Setup(offset = 600, visitCount = 2, stepCount = 2), pi).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              visits {
                matches {
                  datasets {
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

      val matches = on.visits.map { v =>
        Json.obj(
          "datasets" -> Json.obj(
            "matches" -> v.allDatasets.map(did => Json.obj("id" -> did.asJson)).asJson
          )
        )
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> visits -> events") {
    recordAll(mode, Setup(offset = 700, visitCount = 2), pi).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              visits {
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

      val matches = on.visits.map { v =>
        Json.obj(
          "events" -> Json.obj(
            "matches" -> v.allEvents.map(eid => Json.obj("id" -> eid.asJson)).asJson
          )
        )
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> visits -> atomRecords") {
    recordAll(mode, Setup(offset = 800, visitCount = 2, atomCount = 2), pi).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              visits {
                matches {
                  atomRecords {
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

      val matches = on.visits.map { v =>
        Json.obj(
          "atomRecords" -> Json.obj(
            "matches" -> v.atoms.map(a => Json.obj("id" -> a.id.asJson)).asJson
          )
        )
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }
*/
  test("observation -> execution -> atomRecords") {
    recordAll(pi, mode, offset = 0, visitCount = 2, atomCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
                matches {
                  id
                }
              }
            }
          }
        }
      """

      val matches = on.visits.flatMap(_.atoms).map { a =>
        Json.obj("id" -> a.id.asJson)
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "atomRecords": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> atomRecords -> steps") {
    recordAll(pi, mode, offset = 100, visitCount = 2, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
                matches {
                  steps {
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

      val matches = on.visits.flatMap(_.atoms).map { a =>
        Json.obj(
          "steps" -> Json.obj(
            "matches" -> a.steps.map(s => Json.obj("id" -> s.id.asJson)).asJson
          )
        )
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "atomRecords": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> atomRecords -> steps -> datasets") {
    recordAll(pi, mode, offset = 200, atomCount = 2, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
                matches {
                  steps {
                    matches {
                      datasets {
                        matches {
                          id
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      """

      val matches = on.visits.flatMap(_.atoms).map { a =>
        Json.obj(
          "steps" -> Json.obj(
            "matches" -> a.steps.map { s =>
              Json.obj(
                "datasets" -> Json.obj(
                  "matches" -> s.datasets.map { d =>
                    Json.obj("id" -> d.id.asJson)
                  }.asJson
                )
              )
            }.asJson
          )
        )
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "atomRecords": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> atomRecords -> steps -> events") {
    recordAll(pi, mode, offset = 300).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
                matches {
                  steps {
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
          }
        }
      """

      val matches = on.visits.flatMap(_.atoms).map { a =>
        Json.obj(
          "steps" -> Json.obj(
            "matches" -> a.steps.map { s =>
              Json.obj(
                "events" -> Json.obj(
                  "matches" -> s.allEvents.map { e =>
                    Json.obj("id" -> e.asJson)
                  }.asJson
                )
              )
            }.asJson
          )
        )
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "atomRecords": {
              "matches": $matches
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

}
