// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.data.ObservingModeType

class recordVisit extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def recordVisitTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Observation.Id => String,
    expected: Either[Observation.Id => String, Json]
  ): IO[Unit] =

    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      _   <- expect(user, query(oid), expected.leftMap(f => List(f(oid))))
    } yield ()

  test("recordGmosNorthVisit") {

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
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
      staff,
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
      """.stripMargin,
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
      staff,
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
      """.stripMargin,
      ((oid: Observation.Id) => s"Observation $oid not found or is not a GMOS South observation").asLeft
    )

  }
}