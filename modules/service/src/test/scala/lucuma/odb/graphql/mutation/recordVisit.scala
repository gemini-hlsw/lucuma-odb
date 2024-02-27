// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.OdbError
import lucuma.odb.data.ObservingModeType

class recordVisit extends OdbSuite {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordVisitTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Observation.Id => String,
    expected: Either[Observation.Id => String, Json]
  ): IO[Unit] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      _   <- expectSuccessOrOdbError(user, query(oid), expected.leftMap: f =>
        case OdbError.InvalidObservation(_, Some(d)) if d === f(oid) => ()
      ) 
    } yield ()

  test("recordGmosNorthVisit") {

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosNorthVisit(input: {
            observationId: "$oid",
            static: {
              stageMode: NO_FOLLOW
            }
          }) {
            visit {
              static {
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
        "recordGmosNorthVisit": {
          "visit": {
            "static": {
              "stageMode": "NO_FOLLOW",
              "detector": "HAMAMATSU",
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING"
            }
          }
        }
      }
      """.asRight
    )

  }

  test("recordGmosSouthVisit") {

    recordVisitTest(
      ObservingModeType.GmosSouthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosSouthVisit(input: {
            observationId: "$oid",
            static: {
              stageMode: FOLLOW_XY
            }
          }) {
            visit {
              static {
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
            "static": {
              "stageMode": "FOLLOW_XY",
              "detector": "HAMAMATSU",
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING"
            }
          }
        }
      }
      """.asRight
    )

  }

  test("record visit cross site") {

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosSouthVisit(input: {
            observationId: "$oid",
            static: {
              stageMode: FOLLOW_XY,
              detector: HAMAMATSU,
              mosPreImaging: IS_NOT_MOS_PRE_IMAGING
            }
          }) {
            visit {
              static {
                stageMode
              }
            }
          }
        }
      """,
      ((oid: Observation.Id) => s"Observation '$oid' not found or is not a GMOS South observation").asLeft
    )

  }
}
