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
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step

class recordDataset extends OdbSuite {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

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
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: ${sid.asJson},
            filename: "N18630101S0001.fits"
          }) {
            dataset {
              step {
                id
              }
              index
              observation {
                id
              }
              filename
              qaState
              comment
              interval { start }
            }
          }
        }
      """,
      (oid, sid) => json"""
        {
          "recordDataset": {
            "dataset": {
              "step": {
                "id": $sid
              },
              "index": 1,
              "observation": {
                "id": $oid
              },
              "filename": "N18630101S0001.fits",
              "qaState": null,
              "comment": null,
              "interval": null
            }
          }
        }
      """.asRight
    )
  }

  test("recordDataset - init QA state") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: ${sid.asJson},
            filename: "N18630101S0002.fits",
            qaState: USABLE
          }) {
            dataset {
              step {
                id
              }
              index
              observation {
                id
              }
              filename
              qaState
              comment
              interval { start }
            }
          }
        }
      """,
      (oid, sid) => json"""
        {
          "recordDataset": {
            "dataset": {
              "step": {
                "id": $sid
              },
              "index": 1,
              "observation": {
                "id": $oid
              },
              "filename": "N18630101S0002.fits",
              "qaState": "USABLE",
              "comment": null,
              "interval": null
            }
          }
        }
      """.asRight
    )
  }

  test("recordDataset - init comment") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: ${sid.asJson},
            filename: "N18630101S0003.fits",
            comment: "such data"
          }) {
            dataset {
              step {
                id
              }
              index
              observation {
                id
              }
              filename
              qaState
              comment
              interval { start }
            }
          }
        }
      """,
      (oid, sid) => json"""
        {
          "recordDataset": {
            "dataset": {
              "step": {
                "id": $sid
              },
              "index": 1,
              "observation": {
                "id": $oid
              },
              "filename": "N18630101S0003.fits",
              "qaState": null,
              "comment": "such data",
              "interval": null
            }
          }
        }
      """.asRight
    )
  }

  test("recordDataset - reused filename") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: ${sid.asJson},
            filename: "N18630101S0002.fits",
            qaState: USABLE
          }) {
            dataset {
              filename
            }
          }
        }
      """,
      (oid, sid) => "The filename 'N18630101S0002.fits' is already assigned".asLeft
    )
  }

  test("recordDataset - unkown step id") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: "s-d506e5d9-e5d1-4fcc-964c-90afedabc9e8",
            filename: "N18630101S0003.fits"
          }) {
            dataset {
              filename
            }
          }
        }
      """,
      (oid, sid) => "Step id 's-d506e5d9-e5d1-4fcc-964c-90afedabc9e8' not found".asLeft
    )
  }

}
