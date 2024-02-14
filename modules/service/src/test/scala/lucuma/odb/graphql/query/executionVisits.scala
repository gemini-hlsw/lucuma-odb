// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.ObservingModeType

class executionVisits extends OdbSuite with ExecutionQuerySetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

  test("observation -> execution -> visits") {
    recordAll(pi, service, mode, offset = 0, visitCount = 2).flatMap { on =>
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
    recordAll(pi, service, mode, offset = 100, visitCount = 2, stepCount = 2).flatMap { on =>
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
    recordAll(pi, service, mode, offset = 200, visitCount = 2).flatMap { on =>
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
    recordAll(pi, service, mode, offset = 300, visitCount = 2, atomCount = 2).flatMap { on =>
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
    recordAll(pi, service, mode, offset = offset).flatMap { on =>
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

  test("observation -> execution -> visits -> interval") {
    recordAll(pi, service, mode, offset = 550, visitCount = 2, atomCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              visits {
                matches {
                  interval {
                    start
                    end
                    duration { seconds }
                  }
                }
              }
            }
          }
        }
      """

      val matches = on.visits.map { v =>
        val inv = for {
          s <- v.allEvents.headOption.map(_.received)
          e <- v.allEvents.lastOption.map(_.received)
        } yield TimestampInterval.between(s, e)

        inv.fold(Json.Null) { i =>
          Json.obj(
            "interval" -> Json.obj(
              "start"    -> i.start.asJson,
              "end"      -> i.end.asJson,
              "duration" -> Json.obj(
                "seconds" -> i.boundedTimeSpan.toSeconds.asJson
              )
            )
          )
        }
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

  test("empty interval in visit") {
    def query(oid: Observation.Id): String =
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              visits {
                matches {
                  interval {
                    start
                    end
                  }
                }
              }
            }
          }
        }
      """

    val expected = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": [
                {
                  "interval": null
                }
              ]
            }
          }
        }
      }
    """.asRight

    // Set up visit and record the atom and steps, but no events
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, mode.some)
      vid <- recordVisitAs(service, mode.instrument, oid)
      aid <- recordAtomAs(service, mode.instrument, vid)
      sid <- recordStepAs(service, mode.instrument, aid)
      _   <- expect(pi, query(oid), expected)
    } yield ()
  }
}
