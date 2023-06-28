// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.odb.data.ObservingModeType


class addDatasetEvent extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def recordStep(
    mode: ObservingModeType,
    user: User
  ):IO[(Observation.Id, Visit.Id, Step.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      sid <- recordStepAs(user, mode.instrument, vid)
    } yield (oid, vid, sid)

  private def addDatasetEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Step.Id => String,
    expected: (Observation.Id, Visit.Id, Step.Id) => Either[String, Json]
  ): IO[Unit] = {
    for {
      ids <- recordStep(mode, user)
      (oid, vid, sid) = ids
      _   <- expect(user, query(sid), expected(oid, vid, sid).leftMap(s => List(s)))
    } yield ()
}

  test("addDatasetEvent") {
    def query(sid: Step.Id): String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: {
              stepId: "$sid",
              index:  2
            },
            datasetStage: START_WRITE
          }) {
            event {
              visitId
              datasetId {
                stepId
                index
              }
              datasetStage
              observation {
                id
              }
              filename
            }
          }
        }
      """

    addDatasetEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      sid => query(sid),
      (oid, vid, sid) => json"""
      {
        "addDatasetEvent": {
          "event": {
            "visitId": $vid,
            "datasetId": {
              "stepId": $sid,
              "index": 2
            },
            "datasetStage": "START_WRITE",
            "observation": {
              "id": $oid
            },
            "filename": null
          }
        }
      }
      """.asRight
    )

  }

  test("addDatasetEvent - with filename") {
    def query(sid: Step.Id): String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: {
              stepId: "$sid",
              index:  2
            },
            datasetStage: START_WRITE,
            filename: "N20230627S0001.fits"
          }) {
            event {
              visitId
              datasetId {
                stepId
                index
              }
              datasetStage
              observation {
                id
              }
              filename
            }
          }
        }
      """

    addDatasetEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      sid => query(sid),
      (oid, vid, sid) => json"""
      {
        "addDatasetEvent": {
          "event": {
            "visitId": $vid,
            "datasetId": {
              "stepId": $sid,
              "index": 2
            },
            "datasetStage": "START_WRITE",
            "observation": {
              "id": $oid
            },
            "filename": "N20230627S0001.fits"
          }
        }
      }
      """.asRight
    )

  }

  test("addDatasetEvent - unknown step") {
    def query: String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: {
              stepId: "s-cfebc981-db7e-4c35-964d-6b19aa5ed2d7",
              index:  3
            },
            datasetStage: START_WRITE
          }) {
            event {
              datasetId {
                stepId
              }
            }
          }
        }
      """

    addDatasetEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      _ => query,
      (_, _, _) => s"Step id 's-cfebc981-db7e-4c35-964d-6b19aa5ed2d7' not found".asLeft
    )

  }

}