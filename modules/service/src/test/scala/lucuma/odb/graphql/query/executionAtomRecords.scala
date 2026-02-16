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
import lucuma.core.enums.AtomStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SequenceType.Acquisition
import lucuma.core.enums.SequenceType.Science
import lucuma.core.enums.StepGuideState
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.json.gmos.given
import lucuma.odb.json.time.transport.given
import lucuma.odb.json.wavelength.transport.given

class executionAtomRecords extends OdbSuite with ExecutionQuerySetupOperations
                                            with ExecutionTestSupportForGmos {

  val mode    = ObservingModeType.GmosNorthLongSlit

  def noEventSetup: IO[(Observation.Id, Visit.Id, Atom.Id)] =
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, mode.some)
      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      aid <- recordAtomAs(serviceUser, mode.instrument, vid)
    } yield (oid, vid, aid)

  test("observation -> execution -> atomRecords") {
    recordAll(pi, serviceUser, mode, offset = 0, atomCount = 2).flatMap { on =>
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

      val matches = on.visit.atoms.map { a =>
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
    recordAll(pi, serviceUser, mode, offset = 50, stepCount = 2).flatMap { on =>
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

      val matches = on.visit.atoms.map { atom =>
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
                "seconds" -> i.timeSpan.toSeconds.asJson
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
    recordAll(pi, serviceUser, mode, offset = 100, stepCount = 2).flatMap { on =>
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

      val matches = on.visit.atoms.map { a =>
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
    recordAll(pi, serviceUser, mode, offset = 150, stepCount = 2).flatMap { on =>
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

      val matches = on.visit.atoms.map { atom =>
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
                  "seconds" -> i.timeSpan.toSeconds.asJson
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
    recordAll(pi, serviceUser, mode, offset = 200, atomCount = 2, stepCount = 2).flatMap { on =>
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

      val matches = on.visit.atoms.map { a =>
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
    recordAll(pi, serviceUser, mode, offset = 300).flatMap { on =>
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

      val matches = on.visit.atoms.map { a =>
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
    recordAll(pi, serviceUser, mode, offset = offset).flatMap { on =>
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

      val matches = on.visit.atoms.map { a =>
        Json.obj(
          "steps" -> Json.obj(
            "matches" ->
              a.steps.map { _ =>
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
      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      aid <- recordAtomAs(serviceUser, mode.instrument, vid)
      sid <- recordStepAs(serviceUser, mode.instrument, aid)
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
      (o, _, _) <- noEventSetup
      es        <- executionState(o)
    } yield es
    assertIO(res, AtomExecutionState.NotStarted)
  }

  test("execution state - ongoing") {
    val res = for {
      (o, v, a) <- noEventSetup
      _         <- addAtomEventAs(serviceUser, a, v, AtomStage.StartAtom)
      es        <- executionState(o)
    } yield es
    assertIO(res, AtomExecutionState.Ongoing)
  }

  test("execution state - completed") {
    val res = for {
      (o, v, a) <- noEventSetup
      _         <- addAtomEventAs(serviceUser, a, v, AtomStage.StartAtom)
      _         <- addAtomEventAs(serviceUser, a, v, AtomStage.EndAtom)
      es        <- executionState(o)
    } yield es
    assertIO(res, AtomExecutionState.Completed)
  }

  def generatedNextAtomId(user: User, oid: Observation.Id, sequenceType: SequenceType): IO[Atom.Id] =
    query(
      user,
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              config {
                gmosNorth {
                  ${sequenceType.tag} {
                    nextAtom {
                      id
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downFields("observation", "execution", "config", "gmosNorth", sequenceType.tag, "nextAtom", "id")
        .as[Atom.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  test("steps do not change inside of a visit") {
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      ga0 <- generatedNextAtomId(pi, oid, Acquisition)
      aid <- recordAtomAs(serviceUser, mode.instrument, vid, sequenceType = Acquisition)
      sid <- recordStepAs(serviceUser, aid, mode.instrument, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled))
      _   <- addEndStepEvent(sid)
      ga1 <- generatedNextAtomId(pi, oid, Acquisition)
    } yield assertEquals(ga0, ga1)
  }

  test("science steps do not change after switch to acquisition") {
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      ga0 <- generatedNextAtomId(pi, oid, Science)
      aid <- recordAtomAs(serviceUser, mode.instrument, vid, sequenceType = Acquisition)
      sid <- recordStepAs(serviceUser, aid, mode.instrument, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled))
      _   <- addEndStepEvent(sid)
      ga1 <- generatedNextAtomId(pi, oid, Science)
    } yield assertEquals(ga0, ga1)
  }
}
