// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval

class executionVisits extends OdbSuite with ExecutionQuerySetupOperations with ExecutionTestSupportForGmos {

  val mode = ObservingModeType.GmosNorthLongSlit

  test("observation -> execution -> visits") {
    recordAll(pi, serviceUser, mode, offset = 0).flatMap { on =>
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

      val _match = Json.obj("id" -> on.visit.id.asJson)

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": ${List(_match)}
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> visits -> datasets") {
    recordAll(pi, serviceUser, mode, offset = 100, stepCount = 2).flatMap { on =>
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

      val _match =
        Json.obj(
          "datasets" -> Json.obj(
            "matches" -> on.visit.allDatasets.map(did => Json.obj("id" -> did.asJson)).asJson
          )
        )

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": ${List(_match)}
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> visits -> events") {
    recordAll(pi, serviceUser, mode, offset = 200).flatMap { on =>
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

      val _match =
        Json.obj(
          "events" -> Json.obj(
            "matches" -> on.visit.allEvents.map(e => Json.obj("id" -> e.id.asJson)).asJson
          )
        )

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": ${List(_match)}
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> visits -> atomRecords") {
    recordAll(pi, serviceUser, mode, offset = 300, atomCount = 2).flatMap { on =>
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

      val matches = List(
        Json.obj(
          "atomRecords" -> Json.obj(
            "matches" -> on.visit.atoms.map(a => Json.obj("id" -> a.id.asJson)).asJson
          )
        )
      )

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
    recordAll(pi, serviceUser, mode, offset = offset).flatMap { on =>
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

      val _match =
        Json.obj(
          "gmosNorth" -> Json.obj(
            "stageMode" -> "FOLLOW_XY".asJson,
          ),
          "atomRecords" -> Json.obj(
            "matches" -> on.visit.atoms.map(a => Json.obj("id" -> a.id.asJson)).asJson
          )
        )

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": ${List(_match)}
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }

  test("observation -> execution -> visits 1") {
    testInterfaceMapping(400,
      s"""
        gmosNorth {
          stageMode
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
    recordAll(pi, serviceUser, mode, offset = 550, atomCount = 2).flatMap { on =>
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

      val matches =
        val inv = for
          s <- on.visit.allEvents.headOption.map(_.received)
          e <- on.visit.allEvents.lastOption.map(_.received)
        yield TimestampInterval.between(s, e)

        List(inv.fold(Json.Null) { i =>
          Json.obj(
            "interval" -> Json.obj(
              "start"    -> i.start.asJson,
              "end"      -> i.end.asJson,
              "duration" -> Json.obj(
                "seconds" -> i.timeSpan.toSeconds.asJson
              )
            )
          )
        })

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
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationAs(pi, pid, mode.some, tid)
      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      _   <- expect(pi, query(oid), expected)
    yield ()
  }

  test("atom split across visits"):
    def query(oid: Observation.Id): String =
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              visits {
                matches {
                  id
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

    def expected(aid: Atom.Id, vid0: Visit.Id, vid1: Visit.Id): Either[List[String], Json] =
      json"""
        {
          "observation": {
            "execution": {
              "visits": {
                "matches": [
                  {
                    "id": ${vid0.asJson},
                    "atomRecords": {
                      "matches": [
                        {
                          "id": ${aid.asJson}
                        }
                      ]
                    }
                  },
                  {
                    "id": ${vid1.asJson},
                    "atomRecords": {
                      "matches": [
                        {
                          "id": ${aid.asJson}
                        }
                      ]
                    }
                  }
                ]
              }
            }
          }
        }
      """.asRight

    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createObservationAs(pi, pid, mode.some, tid)

      ids  <- scienceSequenceIds(serviceUser, oid).map(_.head)
      aid   = ids._1
      sids  = ids._2

      vid0 <- recordVisitAs(serviceUser, mode.instrument, oid)
      _    <- addEndStepEvent(sids(0), vid0)
      vid1 <- recordVisitAs(serviceUser, mode.instrument, oid)
      _    <- addEndStepEvent(sids(1), vid1)

      _    <- expect(pi, query(oid), expected(aid, vid0, vid1))
    yield ()
}
