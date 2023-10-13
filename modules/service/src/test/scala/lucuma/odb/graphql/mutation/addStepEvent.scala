// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Step
import lucuma.odb.data.ObservingModeType


class addStepEvent extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def recordStep(
    mode: ObservingModeType,
    user: User
  ):IO[(Observation.Id, Step.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
    } yield (oid, sid)

  private def addStepEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Step.Id => String,
    expected: (Observation.Id, Step.Id) => Either[String, Json]
  ): IO[Unit] =
    for {
      ids <- recordStep(mode, user)
      (oid, sid) = ids
      _   <- expect(user, query(sid), expected(oid, sid).leftMap(s => List(s)))
    } yield ()


  test("addStepEvent") {
    def query(sid: Step.Id): String =
      s"""
        mutation {
          addStepEvent(input: {
            stepId:    "$sid",
            stepStage: START_STEP
          }) {
            event {
              stepId
              stepStage
              observation {
                id
              }
            }
          }
        }
      """

    addStepEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      sid => query(sid),
      (oid, sid) => json"""
      {
        "addStepEvent": {
          "event": {
            "stepId": $sid,
            "stepStage": "START_STEP",
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  }

   test("addStepEvent - unknown step") {
    def query: String =
      s"""
        mutation {
          addStepEvent(input: {
            stepId:    "s-cfebc981-db7e-4c35-964d-6b19aa5ed2d7",
            stepStage: START_STEP
          }) {
            event {
              stepId
            }
          }
        }
      """

    addStepEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      _ => query,
      (_, _) => s"Step 's-cfebc981-db7e-4c35-964d-6b19aa5ed2d7' not found".asLeft
    )

  }

}
