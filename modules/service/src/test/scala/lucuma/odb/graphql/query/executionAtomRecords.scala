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
import lucuma.core.enums.AtomStage
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.ObservingModeType

class executionAtomRecords extends OdbSuite with ExecutionQuerySetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

  def noEventSetup: IO[(Observation.Id, Atom.Id)] =
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, mode.some)
      vid <- recordVisitAs(service, mode.instrument, oid)
      aid <- recordAtomAs(service, mode.instrument, vid)
    } yield (oid, aid)

  test("observation -> execution -> atomRecords") {
    recordAll(pi, service, mode, offset = 0, visitCount = 2, atomCount = 2).flatMap { on =>
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

  test("observation -> execution -> atomRecords -> interval") {
    recordAll(pi, service, mode, offset = 50, visitCount = 2, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
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

      val matches = on.visits.flatMap(_.atoms).map { atom =>
        val inv = for {
          s <- atom.allEvents.headOption.map(_.received)
          e <- atom.allEvents.lastOption.map(_.received)
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
    recordAll(pi, service, mode, offset = 100, visitCount = 2, stepCount = 2).flatMap { on =>
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

  test("observation -> execution -> atomRecords -> steps -> interval") {
    recordAll(pi, service, mode, offset = 150, visitCount = 2, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
                matches {
                  steps {
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
          }
        }
      """

      val matches = on.visits.flatMap(_.atoms).map { atom =>
        val stepMatches = atom.steps.map { step =>
          val inv = for {
            s <- step.allEvents.headOption.map(_.received)
            e <- step.allEvents.lastOption.map(_.received)
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

        Json.obj(
          "steps" -> Json.obj(
            "matches" -> stepMatches.asJson
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
    recordAll(pi, service, mode, offset = 200, atomCount = 2, stepCount = 2).flatMap { on =>
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
    recordAll(pi, service, mode, offset = 300).flatMap { on =>
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
                    Json.obj("id" -> e.id.asJson)
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

  // N.B. there is no longer an "interface" involved but the test remains.
  private def testInterfaceMapping(
    offset:       Int,
    matchesQuery: String
  ): IO[Unit] =
    recordAll(pi, service, mode, offset = offset).flatMap { on =>
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
                  "gmosNorth" -> Json.obj(
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

  test("observation -> execution -> atomRecords -> steps") {
    testInterfaceMapping(400,
      s"""
        gmosNorth { fpu { builtin } }
        stepConfig { stepType }
      """
    )
  }

  test("empty interval in atom") {
    def query(oid: Observation.Id): String =
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              atomRecords {
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
            "atomRecords": {
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

  def executionState(oid: Observation.Id): IO[AtomExecutionState] =
    query(
      user = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              atomRecords {
                matches {
                  executionState
                }
              }
            }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downFields("observation", "execution", "atomRecords", "matches")
        .downArray
        .downField("executionState")
        .as[AtomExecutionState]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  test("execution state - not started") {
    val res = for {
      (o, _) <- noEventSetup
      es     <- executionState(o)
    } yield es
    assertIO(res, AtomExecutionState.NotStarted)
  }

  test("execution state - ongoing") {
    val res = for {
      (o, a) <- noEventSetup
      _      <- addAtomEventAs(service, a, AtomStage.StartAtom)
      es     <- executionState(o)
    } yield es
    assertIO(res, AtomExecutionState.Ongoing)
  }

  test("execution state - completed") {
    val res = for {
      (o, a) <- noEventSetup
      _      <- addAtomEventAs(service, a, AtomStage.StartAtom)
      _      <- addAtomEventAs(service, a, AtomStage.EndAtom)
      es     <- executionState(o)
    } yield es
    assertIO(res, AtomExecutionState.Completed)
  }
}
