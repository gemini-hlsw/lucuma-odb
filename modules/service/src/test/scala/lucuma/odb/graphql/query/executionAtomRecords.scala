// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
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

  private def testInterfaceMapping(
    offset:       Int,
    matchesQuery: String
  ): IO[Unit] =
    recordAll(pi, mode, offset = offset).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
                matches {
                  steps {
                    matches {
                      $matchesQuery
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
            "matches" ->
              a.steps.map { s =>
                Json.obj(
                  "instrumentConfig" -> Json.obj(
                    "fpu" -> Json.obj("builtin" -> "LONG_SLIT_0_50".asJson)
                  ),
                  "stepConfig" -> Json.obj(
                    "stepType" -> "SCIENCE".asJson
                  )
                )
              }.asJson
           )
         )
      }.asJson

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

  test("observation -> execution -> atomRecords -> steps (GmosNorthStepRecord) 1") {
    testInterfaceMapping(400,
      s"""
        ... on GmosNorthStepRecord {
          instrumentConfig {
            fpu {
              builtin
            }
          }
          stepConfig {
            stepType
          }
        }
      """
    )
  }

  test("observation -> execution -> atomRecords -> steps (GmosNorthStepRecord) 2") {
    testInterfaceMapping(500,
      s"""
        ... on GmosNorthStepRecord {
          instrumentConfig {
            fpu {
              builtin
            }
          }
        }
        stepConfig {
          stepType
        }
      """
    )
  }

}
