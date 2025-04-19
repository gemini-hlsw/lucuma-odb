// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.OdbError

class recordVisit extends OdbSuite:

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordVisitTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Observation.Id => String,
    expected: Either[Observation.Id => String, Json]
  ): IO[Unit] =
    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      _   <- expectSuccessOrOdbError(user, query(oid), expected.leftMap: f =>
        case OdbError.InvalidObservation(_, Some(d)) if d === f(oid) => ()
      ) 
    yield ()

  test("recordGmosNorthVisit"):

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosNorthVisit(input: {
            observationId: "$oid",
            gmosNorth: {
              stageMode: NO_FOLLOW
            }
          }) {
            visit {
              gmosNorth {
                stageMode
                detector
                mosPreImaging
              }
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      json"""
      {
        "recordGmosNorthVisit": {
          "visit": {
            "gmosNorth": {
              "stageMode": "NO_FOLLOW",
              "detector": "HAMAMATSU",
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING"
            },
            "gmosSouth": null
          }
        }
      }
      """.asRight
    )

  test("recordGmosSouthVisit"):

    recordVisitTest(
      ObservingModeType.GmosSouthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosSouthVisit(input: {
            observationId: "$oid",
            gmosSouth: {
              stageMode: FOLLOW_XYZ
            }
          }) {
            visit {
              gmosSouth {
                stageMode
                detector
                mosPreImaging
              }
            }
          }
        }
      """,
      json"""
      {
        "recordGmosSouthVisit": {
          "visit": {
            "gmosSouth": {
              "stageMode": "FOLLOW_XYZ",
              "detector": "HAMAMATSU",
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING"
            }
          }
        }
      }
      """.asRight
    )

  test("record visit cross site"):

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosSouthVisit(input: {
            observationId: "$oid",
            gmosSouth: {
              stageMode: FOLLOW_XYZ,
              detector: HAMAMATSU,
              mosPreImaging: IS_NOT_MOS_PRE_IMAGING
            }
          }) {
            visit {
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      ((oid: Observation.Id) => s"Observation '$oid' not found or is not a GMOS South observation").asLeft
    )