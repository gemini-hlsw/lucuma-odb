// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.OdbError

class updateDatasets extends OdbSuite with query.DatasetSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)
  val staff   = TestUsers.Standard.staff(4, 44)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service, staff)

  test("updateDatasets") {
    recordDatasets(mode, pi, service, 0, 2, 3).flatMap {
      case (_, _) =>
        val q = s"""
          mutation {
            updateDatasets(input: {
              SET: {
                qaState: PASS
              },
              WHERE: {
                qaState: {
                  IS_NULL: true
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
              .map(f => Json.obj("qaState" -> "PASS".asJson, "filename" -> f.asJson))
            )
          )
        ).asRight

        expect(staff, q, e)
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
    recordDatasets(mode, pi, service, 6, 2, 3).flatMap {
      case (_, _) =>
        val q = s"""
          mutation {
            updateDatasets(input: {
              SET: {
                qaState: FAIL
              },
              WHERE: {
                filename: {
                  LIKE: "N18630101S001_.fits"
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
                "N18630101S0010.fits",
                "N18630101S0011.fits",
                "N18630101S0012.fits"
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
        case OdbError.NotAuthorized(pi.id, _) => () // expected
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
}
