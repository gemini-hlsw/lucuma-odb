// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.User
import lucuma.odb.data.ObservingModeType

class executionStepRecords extends OdbSuite with ExecutionQuerySetupOperations {
  import ExecutionQuerySetupOperations.ObservationNode

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

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
      _  <- setQaState(pi, DatasetQaState.Usable, s"${DatasetFilenamePrefix}0011.fits")
      _  <- expect(pi, qaQuery(on), qaExpected(DatasetQaState.Usable.some))
    } yield ()
  }

  test("qaState - two set") {
    for {
      on <- recordAll(pi, service, mode, offset = 20, datasetCount = 2)
      _  <- setQaState(pi, DatasetQaState.Pass,   s"${DatasetFilenamePrefix}0021.fits")
      _  <- setQaState(pi, DatasetQaState.Usable, s"${DatasetFilenamePrefix}0022.fits")
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
      _  <- setQaState(pi, DatasetQaState.Pass,   s"${DatasetFilenamePrefix}0031.fits")
      _  <- setQaState(pi, DatasetQaState.Usable, s"${DatasetFilenamePrefix}0032.fits")
      _  <- setQaState(pi, DatasetQaState.Fail,   s"${DatasetFilenamePrefix}0033.fits")
      _  <- setQaState(pi, DatasetQaState.Pass,   s"${DatasetFilenamePrefix}0034.fits")
      _  <- expect(pi, qaQuery(on), expected)
    } yield ()
  }

  test("interval - in interface") {
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
      val events = on.visits.head.atoms.head.steps.head.allEvents
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

  test("interval - in fragment") {
    def query(on: ObservationNode): String =
      s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              atomRecords {
                matches {
                  steps {
                    matches {
                      ... on GmosNorthStepRecord {
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
        }
      """

    def expected(on: ObservationNode): Either[List[String], Json] = {
      val events = on.visits.head.atoms.head.steps.head.allEvents
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
      on <- recordAll(pi, service, mode, offset=50)
      _  <- expect(pi, query(on), expected(on))
    } yield ()
  }

}