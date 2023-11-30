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
import lucuma.odb.data.ObservingModeType

class executionVisits extends OdbSuite with ExecutionQuerySetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

  test("observation -> execution -> visits") {
    recordAll(pi, mode, offset = 0, visitCount = 2).flatMap { on =>
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
    recordAll(pi, mode, offset = 100, visitCount = 2, stepCount = 2).flatMap { on =>
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
    recordAll(pi, mode, offset = 200, visitCount = 2).flatMap { on =>
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
            "matches" -> v.allEvents.map(e => Json.obj("id" -> e.id.asJson)).asJson
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
    recordAll(pi, mode, offset = 300, visitCount = 2, atomCount = 2).flatMap { on =>
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

  private def testInterfaceMapping(
    offset:       Int,
    matchesQuery: String
  ): IO[Unit] =
    recordAll(pi, mode, offset = offset).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              visits {
                matches {
                  $matchesQuery
                }
              }
            }
          }
        }
      """

      val matches = on.visits.map { v =>
        Json.obj(
          "static" -> Json.obj(
            "stageMode" -> "FOLLOW_XY".asJson,
          ),
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

  test("observation -> execution -> visits (GmosNorthVisit) 1") {
    testInterfaceMapping(400,
      s"""
        ... on GmosNorthVisit {
          static {
            stageMode
          }
          atomRecords {
            matches {
              id
            }
          }
        }
      """
    )
  }

  test("observation -> execution -> visits (GmosNorthVisit) 2") {
    testInterfaceMapping(500,
      s"""
        ... on GmosNorthVisit {
          static {
            stageMode
          }
        }
        atomRecords {
          matches {
            id
          }
        }
      """
    )
  }

  test("observation -> execution -> visits -> timeChargeInvoice") {
    recordAll(pi, mode, offset = 600, visitCount = 2, atomCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              visits {
                matches {
                  timeChargeInvoice {
                    executionTime {
                      program { seconds }
                      partner { seconds }
                      nonCharged { seconds }
                      total { seconds }
                    }
                    corrections {
                      op
                      amount { seconds }
                    }
                    finalCharge {
                      program { seconds }
                      partner { seconds }
                      nonCharged { seconds }
                      total { seconds }
                    }
                  }
                }
              }
            }
          }
        }
      """

      val matches = on.visits.map { v =>
        json"""
        {
          "timeChargeInvoice": {
            "executionTime": {
              "program": {
                "seconds": 0.000000
              },
              "partner": {
                "seconds": 0.000000
              },
              "nonCharged": {
                "seconds": 0.000000
              },
              "total": {
                "seconds": 0.000000
              }
            },
            "corrections": [],
            "finalCharge": {
              "program": {
                "seconds": 0.000000
              },
              "partner": {
                "seconds": 0.000000
              },
              "nonCharged": {
                "seconds": 0.000000
              },
              "total": {
                "seconds": 0.000000
              }
            }
          }
        }
        """
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

}
