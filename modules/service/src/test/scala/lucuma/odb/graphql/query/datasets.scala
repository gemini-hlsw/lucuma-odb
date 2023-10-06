// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.ObservingModeType

class datasets extends OdbSuite with DatasetSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service).toList

  test("simple datasets selection") {
    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, 0, 2, 3).flatMap {
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
              "N18630101S0001.fits".asJson,
              "N18630101S0002.fits".asJson,
              "N18630101S0003.fits".asJson,
              "N18630101S0004.fits".asJson,
              "N18630101S0005.fits".asJson,
              "N18630101S0006.fits".asJson
            ))
          )
        ).asRight

        expect(pi, q, e)
    }
  }

}
