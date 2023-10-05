// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.effect.IO
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.data.ObservingModeType

class dataset extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service).toList

  private def setup(
    mode: ObservingModeType,
    user: User,
    stepCount: Int = 3,
    datasetsPerStep: Int = 2
  ):IO[(Observation.Id, List[(Step.Id, List[Dataset.Id])])] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      ids <- (0 until stepCount).toList.traverse { x =>
        recordStepAs(user, mode.instrument, aid).flatMap { sid =>
          (0 until datasetsPerStep).toList.traverse { y =>
            recordDatasetAs(user, sid, f"N18630101S${x * datasetsPerStep + y + 1}%04d.fits")
          }.tupleLeft(sid)
        }
      }
    } yield (oid, ids)


  test("pi can select thier own dataset") {
    setup(ObservingModeType.GmosNorthLongSlit, pi, 1, 1).flatMap {
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
}
