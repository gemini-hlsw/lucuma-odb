// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservingModeType

class datasets extends OdbSuite with DatasetSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

  test("simple datasets selection") {
    recordDatasets(mode, pi, service, 0, 2, 3).flatMap {
      case (_, _) =>
        val q = s"""
          query {
            datasets() {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.False,
            "matches" -> Json.fromValues(List(
              "N18630101S0001.fits",
              "N18630101S0002.fits",
              "N18630101S0003.fits",
              "N18630101S0004.fits",
              "N18630101S0005.fits",
              "N18630101S0006.fits"
            ).map(f => Json.obj("filename" -> f.asJson)))
          )
        ).asRight

        expect(pi, q, e)
    }
  }

  test("OFFSET, LIMIT, hasMore") {
    recordDatasets(mode, pi, service, 6, 2, 3).flatMap {
      case (_, steps) =>
        val q = s"""
          query {
            datasets(
              OFFSET: "${steps.head._2.tail.head}",
              LIMIT: 4
            ) {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.True,
            "matches" -> Json.fromValues(List(
              // "N18630101S0007.fits",  skipped
              "N18630101S0008.fits", // offset points to here
              "N18630101S0009.fits",
              "N18630101S0010.fits",
              "N18630101S0011.fits"
              // "N18630101S0012.fits" // after LIMIT
            ).map(f => Json.obj("filename" -> f.asJson)))
          )
        ).asRight

        expect(pi, q, e)
    }
  }

  test("dataset selection") {
    recordDatasets(mode, pi, service, 12, 1, 3).flatMap {
      case (oid, List((_, List(_, did, _)))) =>
        val q = s"""
          query {
            datasets(WHERE: { id: { NEQ: "$did" }, observation: { id: { EQ: "$oid" } } }) {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.False,
            "matches" -> Json.fromValues(List(
              "N18630101S0013.fits",
              // "N18630101S0014.fits", NEQ to this one
              "N18630101S0015.fits"
            ).map(f => Json.obj("filename" -> f.asJson)))
          )
        ).asRight

        expect(pi, q, e)

      case (_, lst) =>
        fail(s"Unexpected result: $lst")
    }
  }

  test("observation selection") {
    recordDatasets(mode, pi, service, 15, 1, 3).flatMap {
      case (oid, _) =>
        val q = s"""
          query {
            datasets(WHERE: { observation: { id: { EQ: "$oid" } } }) {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.False,
            "matches" -> Json.fromValues(List(
              "N18630101S0016.fits",
              "N18630101S0017.fits",
              "N18630101S0018.fits"
            ).map(f => Json.obj("filename" -> f.asJson)))
          )
        ).asRight

        expect(pi, q, e)
    }
  }

  test("step selection") {
    recordDatasets(mode, pi, service, 18, 1, 3).flatMap {
      case (oid, List((sid, _))) =>
        val q = s"""
          query {
            datasets(WHERE: { stepId: { EQ: "$sid" } }) {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.False,
            "matches" -> Json.fromValues(List(
              "N18630101S0019.fits",
              "N18630101S0020.fits",
              "N18630101S0021.fits"
            ).map(f => Json.obj("filename" -> f.asJson)))
          )
        ).asRight

        expect(pi, q, e)

      case (_, lst) =>
        fail(s"Unexpected result: $lst")

    }
  }

  test("step and index selection") {
    recordDatasets(mode, pi, service, 21, 1, 3).flatMap {
      case (oid, List((sid, _))) =>
        val q = s"""
          query {
            datasets(WHERE: { stepId: { EQ: "$sid" }, index: { GT: 1 } }) {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.False,
            "matches" -> Json.fromValues(List(
              // "N18630101S0022.fits", 1'st index skipped
              "N18630101S0023.fits",
              "N18630101S0024.fits"
            ).map(f => Json.obj("filename" -> f.asJson)))
          )
        ).asRight

        expect(pi, q, e)

      case (_, lst) =>
        fail(s"Unexpected result: $lst")

    }
  }

  test("filename") {
    recordDatasets(mode, pi, service, 24, 1, 3).flatMap {
      case (oid, List((sid, _))) =>
        val q = s"""
          query {
            datasets(WHERE: { filename: { LIKE: "N18630101S002%.fits" } }) {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.False,
            "matches" -> Json.fromValues(List(
              "N18630101S0020.fits",
              "N18630101S0021.fits",
              "N18630101S0022.fits",
              "N18630101S0023.fits",
              "N18630101S0024.fits",
              "N18630101S0025.fits",
              "N18630101S0026.fits",
              "N18630101S0027.fits",
            ).map(f => Json.obj("filename" -> f.asJson)))
          )
        ).asRight

        expect(pi, q, e)

      case (_, lst) =>
        fail(s"Unexpected result: $lst")

    }
  }

  test("qaState") {
    recordDatasets(mode, pi, service, 27, 1, 3).flatMap {
      case (oid, List((sid, _))) =>
        val q = s"""
          query {
            datasets(WHERE: { qaState: { IS_NULL: true } }) {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.False,
            "matches" -> Json.fromValues(
              (1 to 30).map { i =>
                f"N18630101S00$i%02d.fits"
              }
              .toList
              .map(f => Json.obj("filename" -> f.asJson))
            )
          )
        ).asRight

        expect(pi, q, e)

      case (_, lst) =>
        fail(s"Unexpected result: $lst")

    }
  }

  test("pi cannot select someone else's dataset") {
    recordDatasets(mode, pi, service, 30, 1, 1).flatMap {
      case _ =>
        val q = s"""
          query {
            datasets() {
              matches {
                filename
              }
            }
          }
        """

        val e = json"""
        {
          "datasets": {
            "matches": [
            ]
          }
        }
        """.asRight

        expect(pi2, q, e)
    }
  }

  test("query via `observation` -> `execution` -> `datasets`") {
    recordDatasets(mode, pi, service, 31, 1, 3).flatMap {
      case (oid, _) =>
        val q = s"""
          query {
            observation(observationId: "$oid") {
              execution {
                datasets() {
                  matches {
                    filename
                  }
                }
              }
            }
          }
        """

        val e = json"""
        {
          "observation": {
            "execution": {
              "datasets": {
                "matches": [
                  {
                    "filename": "N18630101S0032.fits"
                  },
                  {
                    "filename": "N18630101S0033.fits"
                  },
                  {
                    "filename": "N18630101S0034.fits"
                  }
                ]
              }
            }
          }
        }
        """.asRight

        expect(pi, q, e)
    }
  }

  test("isWritten selection"):
    recordDatasets(mode, pi, service, 34, 1, 3).flatMap {
      case (_, List((_, List(did0, did1, _)))) =>

        val q = s"""
          query {
            datasets(WHERE: { isWritten: { EQ: true } }) {
              hasMore
              matches {
                filename
              }
            }
          }
        """

        val e = Json.obj(
          "datasets" -> Json.obj(
            "hasMore" -> Json.False,
            "matches" -> Json.fromValues(List(
              "N18630101S0035.fits"
            ).map(f => Json.obj("filename" -> f.asJson)))
          )
        ).asRight

        for
          _ <- addDatasetEventAs(service, did0, DatasetStage.StartExpose)
          _ <- addDatasetEventAs(service, did0, DatasetStage.EndWrite)
          _ <- addDatasetEventAs(service, did1, DatasetStage.StartExpose)
          _ <- expect(pi, q, e)
        yield ()

      case _ =>
        sys.error("Expected 1 step, 3 datasets.")
    }

}
