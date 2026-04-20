// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.core.model.User


class updateObservations_GhostIfu extends OdbSuite with UpdateObservationsOps with ObservingModeSetupOperations:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi)

  val GhostIfuInput: String = s"""
    ghostIfu: {
      resolutionMode: STANDARD
      red: {
        exposureTimeMode: {
          timeAndCount: {
            time: { seconds: 10.0 }
            count: 10
            at: { nanometers: 500 }
          }
        }
        explicitBinning: ONE_BY_TWO
      }
      blue: {
        exposureTimeMode: {
          timeAndCount: {
            time: { seconds: 20.0 }
            count: 20
            at: { nanometers: 500 }
          }
        }
        explicitReadMode: FAST
      }
      explicitIfu1Agitator: ENABLED
    }
  """

  test("Update exposure time modes"):
    val update = """
      observingMode: {
        ghostIfu: {
          red: {
            exposureTimeMode: {
              timeAndCount: {
                time: { seconds: 30.0 }
                count: 30
                at: { nanometers: 500 }
              }
            }
          }
          blue: {
            exposureTimeMode: {
              timeAndCount: {
                time: { seconds: 40.0 }
                count: 40
                at: { nanometers: 500 }
              }
            }
          }
        }
      }
    """

    val query = """
      observations {
        observingMode {
          ghostIfu {
            red {
              exposureTimeMode {
                timeAndCount {
                  time { seconds }
                  count
                }
              }
            }
            blue {
              exposureTimeMode {
                timeAndCount {
                  time { seconds }
                  count
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "ghostIfu": {
                  "red": {
                    "exposureTimeMode": {
                      "timeAndCount": {
                        "time": { "seconds": 30.000000 },
                        "count": 30
                      }
                    }
                  },
                  "blue": {
                    "exposureTimeMode": {
                      "timeAndCount": {
                        "time": { "seconds": 40.000000 },
                        "count": 40
                      }
                    }
                  }
                }
              }
            }
          ]
        }
      }
    """.asRight

    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), GhostIfuInput)
      _ <- updateObservation(pi, o, update, query, expected)
    yield o