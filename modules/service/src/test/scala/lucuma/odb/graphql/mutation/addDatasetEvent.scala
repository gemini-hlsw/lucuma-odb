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
import lucuma.core.model.sequence.Dataset
import lucuma.odb.data.ObservingModeType


class addDatasetEvent extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def recordDataset(
    mode: ObservingModeType,
    user: User,
    file: String
  ):IO[(Observation.Id, Dataset.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
      did <- recordDatasetAs(user, sid, file)
    } yield (oid, did)

  private def addDatasetEventTest(
    mode:     ObservingModeType,
    user:     User,
    file:     String,
    query:    Dataset.Id => String,
    expected: (Observation.Id, Dataset.Id) => Either[String, Json]
  ): IO[Unit] = {
    for {
      ids <- recordDataset(mode, user, file)
      (oid, did) = ids
      _   <- expect(user, query(did), expected(oid, did).leftMap(s => List(s)))
    } yield ()
}

  test("addDatasetEvent") {
    def query(did: Dataset.Id): String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: "$did",
            datasetStage: START_WRITE
          }) {
            event {
              datasetStage
              dataset {
                id
              }
              observation {
                id
              }
            }
          }
        }
      """

    addDatasetEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      "N18630101S0001.fits",
      did => query(did),
      (oid, did) => json"""
      {
        "addDatasetEvent": {
          "event": {
            "datasetStage": "START_WRITE",
            "dataset": {
              "id": $did
            },
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  }

  test("addDatasetEvent - with filename") {
    def query(did: Dataset.Id): String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: "$did",
            datasetStage: START_WRITE
          }) {
            event {
              datasetStage
              dataset {
                id
              }
              observation {
                id
              }
            }
          }
        }
      """

    addDatasetEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      "N18630101S0002.fits",
      did => query(did),
      (oid, did) => json"""
      {
        "addDatasetEvent": {
          "event": {
            "datasetStage": "START_WRITE",
            "dataset": {
              "id": $did
            },
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  }

  test("addDatasetEvent - unknown dataset") {
    def query: String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: "d-1863",
            datasetStage: START_WRITE
          }) {
            event {
              dataset {
                id
              }
            }
          }
        }
      """

    addDatasetEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      "N18630101S0003.fits",
      _ => query,
      (_, _) => s"Dataset 'd-1863' not found".asLeft
    )

  }

}
