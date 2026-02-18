// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.ObservingModeType
import lucuma.odb.data.OdbError

class updateDatasets extends OdbSuite with query.DatasetSetupOperations with query.ExecutionTestSupportForGmos:

  val mode = ObservingModeType.GmosNorthLongSlit

  test("updateDatasets") {
    recordDatasets(mode, pi, serviceUser, 0, 2, 3).flatMap {
      case (_, _) =>
        val q = s"""
          mutation {
            updateDatasets(input: {
              SET: {
                qaState: PASS,
                comment: "very pass"
              },
              WHERE: {
                qaState: {
                  IS_NULL: true
                }
              }
            }) {
              datasets {
                qaState
                comment
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "updateDatasets" -> Json.obj(
            "datasets"     -> Json.fromValues(
              (1 to 6).map { i =>
                f"N18630101S00$i%02d.fits"
              }
              .toList
              .map(f => Json.obj("qaState" -> "PASS".asJson, "comment" -> "very pass".asJson, "filename" -> f.asJson))
            )
          )
        ).asRight

        expect(staff, q, e)
    }
  }

  test("updateDatasets - where comment") {
    recordDatasets(mode, pi, serviceUser, 6, 2, 3).flatMap {
      case (_, _) =>
        val init = query(staff, s"""
          mutation {
            updateDatasets(input: {
              SET: {
                comment: "such pass"
              },
              WHERE: {
                comment: { IS_NULL: true }
              }
            }) {
              datasets { comment }
            }
          }
        """)

        val q = s"""
          mutation {
            updateDatasets(input: {
              SET: {
                comment: "such very pass"
              },
              WHERE: {
                comment: { LIKE: "such %" }
              }
            }) {
              datasets {
                comment
              }
            }
          }
        """

        val e = Json.obj(
          "updateDatasets" -> Json.obj(
            "datasets"     -> Json.fromValues(
              (7 to 12).map { i =>
                f"N18630101S00$i%02d.fits"
              }
              .toList
              .map(_ => Json.obj("comment" -> "such very pass".asJson))
            )
          )
        ).asRight

        init *> expect(staff, q, e)
    }
  }


  test("updateDatasets - unset") {
      val q = s"""
        mutation {
          updateDatasets(input: {
            SET: {
              qaState: null
            },
            WHERE: {
              qaState: {
                IS_NULL: false
              }
            }
          }) {
            datasets {
              qaState
              filename
            }
          }
        }
      """

      val e = Json.obj(
        "updateDatasets" -> Json.obj(
          "datasets"     -> Json.fromValues(
            (1 to 6).map { i =>
              f"N18630101S00$i%02d.fits"
            }
            .toList
            .map(f => Json.obj("qaState" -> None.asJson, "filename" -> f.asJson))
          )
        )
      ).asRight

      expect(staff, q, e)
  }

  test("updateDatasets - subset select") {
    recordDatasets(mode, pi, serviceUser, 17, 2, 3).flatMap {
      case (_, _) =>
        val q = s"""
          mutation {
            updateDatasets(input: {
              SET: {
                qaState: FAIL
              },
              WHERE: {
                filename: {
                  LIKE: "N18630101S002_.fits"
                }
              }
            }) {
              datasets {
                qaState
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "updateDatasets" -> Json.obj(
            "datasets"     -> Json.fromValues(
              List(
                "N18630101S0020.fits",
                "N18630101S0021.fits",
                "N18630101S0022.fits",
                "N18630101S0023.fits"
              ).map(f => Json.obj("qaState" -> "FAIL".asJson, "filename" -> f.asJson))
            )
          )
        ).asRight

        expect(staff, q, e)
    }
  }

  test("updateDatasets - access check") {
      val q = s"""
        mutation {
          updateDatasets(input: {
            SET: {
              qaState: USABLE
            },
            WHERE: {
              filename: {
                LIKE: "N18630101S00%.fits"
              }
            }
          }) {
            datasets {
              qaState
              filename
            }
          }
        }
      """
      expectOdbError(pi, q, {
        case OdbError.NotAuthorized(_, _) => () // expected
      })
  }

  test("updateDatasets - LIMIT") {
      val q = s"""
        mutation {
          updateDatasets(input: {
            SET: {
              qaState: USABLE
            },
            WHERE: {
              filename: {
                LIKE: "N18630101S00%.fits"
              }
            },
            LIMIT: 1
          }) {
            hasMore
            datasets {
              qaState
              filename
            }
          }
        }
      """

      val e = Json.obj(
        "updateDatasets" -> Json.obj(
          "hasMore"  -> true.asJson,
          "datasets" -> List(
            Json.obj(
              "qaState"  -> "USABLE".asJson,
              "filename" -> "N18630101S0001.fits".asJson
            )
          ).asJson
        )
      ).asRight

      expect(staff, q, e)
  }

  test("chronicle auditing"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationAs(pi, pid, mode.some, tid)
      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      sid <- scienceSequenceIds(serviceUser, oid).map(_.head._2.head)
      did <- recordDatasetAs(serviceUser, sid, vid, "N18630703S0001.fits")
      _   <- updateDatasets(staff, DatasetQaState.Pass, List(did))
      _   <- assertIO(chronDatasetUpdates(did).map(_.drop(1)), List(
          json"""
            {
              "c_user"                      : ${staff.id},
              "c_operation"                 : "UPDATE",
              "c_dataset_id"                : $did,
              "c_mod_dataset_id"            : false,
              "c_mod_step_id"               : false,
              "c_mod_file_site"             : false,
              "c_mod_file_date"             : false,
              "c_mod_file_index"            : false,
              "c_mod_filename"              : false,
              "c_mod_qa_state"              : true,
              "c_mod_start_time"            : false,
              "c_mod_end_time"              : false,
              "c_mod_observation_id"        : false,
              "c_mod_visit_id"              : false,
              "c_mod_observation_reference" : false,
              "c_mod_step_index"            : false,
              "c_mod_exposure_index"        : false,
              "c_mod_dataset_reference"     : false,
              "c_mod_comment"               : false,
              "c_new_dataset_id"            : null,
              "c_new_step_id"               : null,
              "c_new_file_site"             : null,
              "c_new_file_date"             : null,
              "c_new_file_index"            : null,
              "c_new_filename"              : null,
              "c_new_qa_state"              : "Pass",
              "c_new_start_time"            : null,
              "c_new_end_time"              : null,
              "c_new_observation_id"        : null,
              "c_new_visit_id"              : null,
              "c_new_observation_reference" : null,
              "c_new_step_index"            : null,
              "c_new_exposure_index"        : null,
              "c_new_dataset_reference"     : null,
              "c_new_comment"               : null
            }
          """
      ))
    yield ()