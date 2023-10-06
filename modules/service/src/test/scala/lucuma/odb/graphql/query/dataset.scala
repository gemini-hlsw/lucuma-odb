// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.data.ObservingModeType

class dataset extends OdbSuite with DatasetSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service).toList

  test("pi can select thier own dataset") {
    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, 0, 1, 1).flatMap {
      case (oid, List((sid, List(did)))) =>
        val q = s"""
          query {
            dataset(datasetId: {
              stepId: "$sid",
              index: 1
            }) {
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

  test("pi cannot select someone else's dataset") {
    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, 1, 1, 1).flatMap {
      case (oid, List((sid, List(did)))) =>
        val q = s"""
          query {
            dataset(datasetId: {
              stepId: "$sid",
              index: 2
            }) {
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
}
