// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.IdempotencyKey

class recordDataset extends OdbSuite with query.ExecutionTestSupportForGmos {

  private def setup(
    mode: ObservingModeType,
    user: User
  ): IO[(Program.Id, Observation.Id, Visit.Id, Atom.Id, Step.Id)] =
    for
      pid <- createProgramAs(user)
      tid <- createTargetWithProfileAs(user, pid)
      oid <- createObservationAs(user, pid, mode.some, tid)
      vid <- recordVisitAs(user, mode.instrument, oid)
      ids <- scienceSequenceIds(user, oid).map(_.head)
      aid  = ids._1
      sid  = ids._2.head
      _   <- addEndStepEvent(sid, vid)
    yield (pid, oid, vid, aid, sid)

  private def recordDatasetTest(
    mode:     ObservingModeType,
    user:     User,
    query:    (Step.Id, Visit.Id) => String,
    expected: (Observation.Id, Step.Id) => Either[String, Json]
  ): IO[Unit] =
    for {
      ids <- setup(mode, user)
      (_, oid, vid, _, sid) = ids
      _   <- expect(user, query(sid, vid), expected(oid, sid).leftMap(msg => List(msg)))
    } yield ()

  test("recordDataset") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      serviceUser,
      (sid, vid) => s"""
        mutation {
          recordDataset(input: {
            stepId: "$sid"
            visitId: "$vid"
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
      serviceUser,
      (sid, vid) => s"""
        mutation {
          recordDataset(input: {
            stepId: "$sid"
            visitId: "$vid"
            filename: "N18630101S0002.fits"
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
      serviceUser,
      (sid, vid) => s"""
        mutation {
          recordDataset(input: {
            stepId: "$sid"
            visitId: "$vid"
            filename: "N18630101S0003.fits"
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
      serviceUser,
      (sid, vid) => s"""
        mutation {
          recordDataset(input: {
            stepId: "$sid"
            visitId: "$vid"
            filename: "N18630101S0002.fits"
            qaState: USABLE
          }) {
            dataset {
              filename
            }
          }
        }
      """,
      (_, _) => "The filename 'N18630101S0002.fits' is already assigned: Key (c_file_site, c_file_date, c_file_index)=(gn, 1863-01-01, 2)".asLeft
    )
  }

  test("recordDataset - unkown step id") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      serviceUser,
      (_, vid) => s"""
        mutation {
          recordDataset(input: {
            stepId: "s-d506e5d9-e5d1-4fcc-964c-90afedabc9e8"
            visitId: "$vid"
            filename: "N18630101S0003.fits"
          }) {
            dataset {
              filename
            }
          }
        }
      """,
      (_, _) => "Step id 's-d506e5d9-e5d1-4fcc-964c-90afedabc9e8' not found".asLeft
    )
  }

  test("chronicle auditing"):
    setup(ObservingModeType.GmosNorthLongSlit, serviceUser).flatMap: (_, oid, vid, _, sid) =>
      recordDatasetAs(serviceUser, sid, vid, "N18630101S0004.fits").flatMap: did =>
        assertIO(chronDatasetUpdates(did), List(
          json"""
            {
              "c_user"                      : ${serviceUser.id},
              "c_operation"                 : "INSERT",
              "c_dataset_id"                : $did,
              "c_mod_dataset_id"            : true,
              "c_mod_step_id"               : true,
              "c_mod_file_site"             : true,
              "c_mod_file_date"             : true,
              "c_mod_file_index"            : true,
              "c_mod_filename"              : true,
              "c_mod_qa_state"              : false,
              "c_mod_start_time"            : false,
              "c_mod_end_time"              : false,
              "c_mod_observation_id"        : true,
              "c_mod_visit_id"              : true,
              "c_mod_observation_reference" : false,
              "c_mod_step_index"            : true,
              "c_mod_exposure_index"        : true,
              "c_mod_dataset_reference"     : false,
              "c_mod_comment"               : false,
              "c_new_dataset_id"            : $did,
              "c_new_step_id"               : $sid,
              "c_new_file_site"             : "gn",
              "c_new_file_date"             : "1863-01-01",
              "c_new_file_index"            : 4,
              "c_new_filename"              : "N18630101S0004.fits",
              "c_new_qa_state"              : null,
              "c_new_start_time"            : null,
              "c_new_end_time"              : null,
              "c_new_observation_id"        : $oid,
              "c_new_visit_id"              : $vid,
              "c_new_observation_reference" : null,
              "c_new_step_index"            : 1,
              "c_new_exposure_index"        : 1,
              "c_new_dataset_reference"     : null,
              "c_new_comment"               : null
            }
          """
        ))

  test("recordDataset - idempotencyKey"):
    val idm = IdempotencyKey.FromString.getOption("7304956b-45ab-45b6-8db1-ae6f743b519c").get

    def recordDataset(sid: Step.Id, vid: Visit.Id): IO[(Dataset.Id, IdempotencyKey)] =
      query(
        user  = serviceUser,
        query = s"""
          mutation {
            recordDataset(input: {
              stepId: "$sid"
              visitId: "$vid"
              filename: "N18630101S0006.fits"
              idempotencyKey: "${IdempotencyKey.FromString.reverseGet(idm)}"
            }) {
              dataset {
                id
                idempotencyKey
              }
            }
          }
        """
      ).map: js =>
        val d = js.hcursor.downFields("recordDataset", "dataset")
        (
          d.downField("id").require[Dataset.Id],
          d.downField("idempotencyKey").require[IdempotencyKey]
        )

    assertIOBoolean:
      for
        (_, _, v, _, s) <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
        (d0, k0)        <- recordDataset(s, v)
        (d1, k1)        <- recordDataset(s, v)
      yield (d0 === d1) && (k0 === idm) && (k1 === idm)

}
