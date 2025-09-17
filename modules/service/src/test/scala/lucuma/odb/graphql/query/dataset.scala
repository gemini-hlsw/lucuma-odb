// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.TimestampInterval

class dataset extends OdbSuite with DatasetSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service).toList

  test("pi can select thier own dataset") {
    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 0, 1, 1).flatMap {
      case (oid, List((_, List(did)))) =>
        val q = s"""
          query {
            dataset(datasetId: "$did") {
              filename
            }
          }
        """

        val e = json"""
        {
          "dataset": {
            "filename": "N18630101S0001.fits"
          }
        }
        """.asRight

        expect(pi, q, e)

      case _ =>
        fail("expected a single step and single dataset")
    }
  }

  test("empty interval when not complete") {
    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 100, 1, 1).flatMap {
      case (oid, List((_, List(did)))) =>
        val q = s"""
          query {
            dataset(datasetId: "$did") {
              interval {
                start
              }
            }
          }
        """

        val e = json"""
        {
          "dataset": {
            "interval": null
          }
        }
        """.asRight

        expect(pi, q, e)

      case _ =>
        fail("expected a single step and single dataset")
    }
  }

  test("dataset interval") {
    val f = for {
      ds <- recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 200, 1, 1)
      (_, List((_, List(did)))) = ds
      s  <- addDatasetEventAs(service, did, DatasetStage.StartExpose)
      e  <- addDatasetEventAs(service, did, DatasetStage.EndWrite)
    } yield (did, TimestampInterval.between(s.received, e.received))

    f.flatMap { (did, inv) =>
        val q = s"""
          query {
            dataset(datasetId: "$did") {
              interval {
                start
                end
                duration { seconds }
              }
            }
          }
        """

        val e = json"""
        {
          "dataset": {
            "interval": {
              "start": ${inv.start.asJson},
              "end": ${inv.end.asJson},
              "duration": {
                "seconds": ${inv.timeSpan.toSeconds.asJson}
              }
            }
          }
        }
        """.asRight

        expect(pi, q, e)
    }
  }

  test("pi cannot select someone else's dataset") {
    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 300, 1, 1).flatMap {
      case (oid, List((_, List(did)))) =>
        val q = s"""
          query {
            dataset(datasetId: "$did") {
              filename
            }
          }
        """

        val e = json"""
        {
          "dataset": null
        }
        """.asRight

        expect(pi2, q, e)

      case _ =>
        fail("expected a single step and single dataset")
    }
  }

  test("isWritten"):
    def isWritten(did: Dataset.Id): IO[Boolean] =
      query(
        user  = pi,
        query = s"""
          query { dataset(datasetId: "$did") { isWritten } }
        """
      ).map: json =>
        json.hcursor.downFields("dataset", "isWritten").require[Boolean]

    val res = for
      ds <- recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 400, 1, 1)
      (_, List((_, List(did)))) = ds
      s  <- addDatasetEventAs(service, did, DatasetStage.StartExpose)
      w0 <- isWritten(did)
      e  <- addDatasetEventAs(service, did, DatasetStage.EndWrite)
      w1 <- isWritten(did)
    yield (w0, w1)

    assertIO(res, (false, true))

}
