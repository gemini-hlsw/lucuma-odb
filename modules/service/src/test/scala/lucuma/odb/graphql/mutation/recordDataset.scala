// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.odb.data.ObservingModeType

class recordDataset extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def setup(
    mode: ObservingModeType,
    user: User
  ): IO[(Program.Id, Observation.Id, Visit.Id, Atom.Id, Step.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
    } yield (pid, oid, vid, aid, sid)

  private def recordDatasetTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Step.Id => String,
    expected: (Observation.Id, Step.Id) => Either[String, Json]
  ): IO[Unit] =
    for {
      ids <- setup(mode, user)
      (_, oid, _, _, sid) = ids
      _   <- expect(user, query(sid), expected(oid, sid).leftMap(msg => List(msg)))
    } yield ()

  test("recordDataset") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: ${sid.asJson},
            filename: "N18630101S0001.fits"
          }) {
            dataset {
              id {
                stepId
                index
              }
              observation {
                id
              }
              filename
              qaState
              start
              end
            }
          }
        }
      """,
      (oid, sid) => json"""
        {
          "recordDataset": {
            "dataset": {
              "id": {
                "stepId": $sid,
                "index": 1
              },
              "observation": {
                "id": $oid
              },
              "filename": "N18630101S0001",
              "qaState": null,
              "start": null,
              "end": null
            }
          }
        }
      """.asRight
    )
  }

}
