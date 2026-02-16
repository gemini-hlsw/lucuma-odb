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
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.odb.data.StepExecutionState

class executionStepRecords extends OdbSuite with ExecutionQuerySetupOperations {
  import ExecutionQuerySetupOperations.ObservationNode

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)
  val staff   = TestUsers.Standard.staff(4, 44)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service, staff).toList

  def qaQuery(on: ObservationNode): String =
    s"""
      query {
        observation(observationId: "${on.id}") {
          execution {
            atomRecords {
              matches {
                steps {
                  matches {
                    qaState
                  }
                }
              }
            }
          }
        }
      }
    """

  def qaExpected(qa: Option[DatasetQaState]): Either[List[String], Json] =
    json"""
      {
        "observation": {
          "execution": {
            "atomRecords": {
              "matches": [
                {
                  "steps": {
                    "matches": [
                      {
                        "qaState": ${qa.asJson}
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


  test("qaState - unset") {
    recordAll(pi, service, mode, offset = 0, datasetCount = 2).flatMap { on =>
      expect(pi, qaQuery(on), qaExpected(none))
    }
  }

  test("qaState - one set") {
    for {
      on <- recordAll(pi, service, mode, offset = 10, datasetCount = 2)
      _  <- setQaState(staff, DatasetQaState.Usable, s"${DatasetFilenamePrefix}0011.fits")
      _  <- expect(pi, qaQuery(on), qaExpected(DatasetQaState.Usable.some))
    } yield ()
  }

  test("qaState - two set") {
    for {
      on <- recordAll(pi, service, mode, offset = 20, datasetCount = 2)
      _  <- setQaState(staff, DatasetQaState.Pass,   s"${DatasetFilenamePrefix}0021.fits")
      _  <- setQaState(staff, DatasetQaState.Usable, s"${DatasetFilenamePrefix}0022.fits")
      _  <- expect(pi, qaQuery(on), qaExpected(DatasetQaState.Usable.some))
    } yield ()
  }

  test("qaState - two steps, two set") {
    val expected = json"""
      {
        "observation": {
          "execution": {
            "atomRecords": {
              "matches": [
                {
                  "steps": {
                    "matches": [
                      {
                        "qaState": "USABLE"
                      },
                      {
                        "qaState": "FAIL"
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

    for {
      on <- recordAll(pi, service, mode, offset = 30, stepCount = 2, datasetCount = 2)
      _  <- setQaState(staff, DatasetQaState.Pass,   s"${DatasetFilenamePrefix}0031.fits")
      _  <- setQaState(staff, DatasetQaState.Usable, s"${DatasetFilenamePrefix}0032.fits")
      _  <- setQaState(staff, DatasetQaState.Fail,   s"${DatasetFilenamePrefix}0033.fits")
      _  <- setQaState(staff, DatasetQaState.Pass,   s"${DatasetFilenamePrefix}0034.fits")
      _  <- expect(pi, qaQuery(on), expected)
    } yield ()
  }

  test("interval") {
    def query(on: ObservationNode): String =
      s"""
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
                      }
                    }
                  }
                }
              }
            }
          }
        }
      """

    def expected(on: ObservationNode): Either[List[String], Json] = {
      val events = on.visit.atoms.head.steps.head.allEvents
      val start  = events.head.received
      val end    = events.last.received
      json"""
        {
          "observation": {
            "execution": {
              "atomRecords": {
                "matches": [
                  {
                    "steps": {
                      "matches": [
                        {
                          "interval": {
                            "start": ${start.asJson},
                            "end": ${end.asJson}
                          }
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
    }


    for {
      on <- recordAll(pi, service, mode, offset=40)
      _  <- expect(pi, query(on), expected(on))
    } yield ()
  }

  def noEventSetup: IO[(Observation.Id, Step.Id, Visit.Id)] =
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, mode.some)
      vid <- recordVisitAs(service, mode.instrument, oid)
      aid <- recordAtomAs(service, mode.instrument, vid)
      sid <- recordStepAs(service, mode.instrument, aid)
    } yield (oid, sid, vid)

  test("empty interval in step") {
    def query(oid: Observation.Id): String =
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              atomRecords {
                matches {
                  steps {
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
                  "steps": {
                    "matches": [
                      {
                        "interval": null
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

    // Set up visit and record the atom and steps, but no events
    for {
      (o, _, _) <- noEventSetup
      _         <- expect(pi, query(o), expected)
    } yield ()
  }


  def executionState(oid: Observation.Id): IO[StepExecutionState] =
    query(
      user = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              atomRecords {
                matches {
                  steps {
                    matches {
                      executionState
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
        .downFields("observation", "execution", "atomRecords", "matches")
        .downArray
        .downFields("steps", "matches")
        .downArray
        .downField("executionState")
        .as[StepExecutionState]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  test("execution state - not started") {
    val res = for {
      (o, _, _) <- noEventSetup
      es        <- executionState(o)
    } yield es
    assertIO(res, StepExecutionState.NotStarted)
  }

  test("execution state - ongoing") {
    val res = for {
      (o, s, v) <- noEventSetup
      _         <- addStepEventAs(service, s, v, StepStage.StartStep)
      es        <- executionState(o)
    } yield es
    assertIO(res, StepExecutionState.Ongoing)
  }

  test("execution state - abort") {
    val res = for {
      (o, s, v) <- noEventSetup
      _         <- addStepEventAs(service, s, v, StepStage.StartStep)
      _         <- addStepEventAs(service, s, v, StepStage.Abort)
      es        <- executionState(o)
    } yield es
    assertIO(res, StepExecutionState.Aborted)
  }

  test("execution state - completed") {
    val res = for {
      (o, s, v) <- noEventSetup
      _         <- addStepEventAs(service, s, v, StepStage.StartStep)
      _         <- addStepEventAs(service, s, v, StepStage.EndStep)
      es        <- executionState(o)
    } yield es
    assertIO(res, StepExecutionState.Completed)
  }

  test("execution state - stopped") {
    val res = for {
      (o, s, v) <- noEventSetup
      _         <- addStepEventAs(service, s, v, StepStage.StartStep)
      _         <- addStepEventAs(service, s, v, StepStage.Stop)
      es        <- executionState(o)
    } yield es
    assertIO(res, StepExecutionState.Stopped)
  }

  test("telescopeConfig mapping works") {
    for
      on <- recordAll(pi, service, mode, offset = 500)
      _  <- expect(
              pi,
              s"""
                query {
                  observation(observationId: "${on.id}") {
                    execution {
                      atomRecords {
                        matches {
                          steps {
                            matches {
                              telescopeConfig {
                                offset { q { arcseconds } }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              """,
              json"""
                {
                  "observation": {
                    "execution": {
                      "atomRecords": {
                        "matches": [
                          {
                            "steps": {
                              "matches": [
                                {
                                  "telescopeConfig": {
                                    "offset": { "q": { "arcseconds": 10 } }
                                  }
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
            )
    yield ()
  }

}
