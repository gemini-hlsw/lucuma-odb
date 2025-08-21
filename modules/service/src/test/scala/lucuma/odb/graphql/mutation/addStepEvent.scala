// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.AtomStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepStage
import lucuma.core.model.Client
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Step
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.StepExecutionState


class addStepEvent extends OdbSuite with ExecutionState {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

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
              step {
                id
              }
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
      service,
      sid => query(sid),
      (oid, sid) => json"""
      {
        "addStepEvent": {
          "event": {
            "step": {
               "id": $sid
            },
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
              step {
                id
              }
            }
          }
        }
      """

    addStepEventTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      _ => query,
      (_, _) => s"Step 's-cfebc981-db7e-4c35-964d-6b19aa5ed2d7' not found".asLeft
    )
  }

  test("addStepEvent - once terminal, state doesn't change") {
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit

    import StepExecutionState.*

    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
      _   <- addAtomEventAs(user, aid, AtomStage.StartAtom)
      _   <- addStepEventAs(user, sid, StepStage.StartStep)
      _   <- addStepEventAs(user, sid, StepStage.Abort)
      _   <- addStepEventAs(user, sid, StepStage.EndStep) // won't change state
      res <- stepExecutionState(user, oid)
    } yield assertEquals(res, List(Aborted))
  }

  test("addStepEvent - abandon step") {
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit

    import StepExecutionState.*

    for {
      pid  <- createProgramAs(user)
      oid  <- createObservationAs(user, pid, mode.some)
      vid  <- recordVisitAs(user, mode.instrument, oid)
      aid0 <- recordAtomAs(user, mode.instrument, vid)
      sid0 <- recordStepAs(user, mode.instrument, aid0)
      sid1 <- recordStepAs(user, mode.instrument, aid0)
      sid2 <- recordStepAs(user, mode.instrument, aid0)
      _    <- addAtomEventAs(user, aid0, AtomStage.StartAtom)
      _    <- addStepEventAs(user, sid0, StepStage.StartStep)
      _    <- addStepEventAs(user, sid1, StepStage.StartStep)
      _    <- addStepEventAs(user, sid1, StepStage.EndStep)
      _    <- addStepEventAs(user, sid2, StepStage.StartStep)
      res  <- stepExecutionState(user, oid)
    } yield assertEquals(res, List(Abandoned, Completed, Ongoing))
  }

  test("addStepEvent - abandon atom") {
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit

    for {
      pid  <- createProgramAs(user)
      oid  <- createObservationAs(user, pid, mode.some)
      vid  <- recordVisitAs(user, mode.instrument, oid)
      aid0 <- recordAtomAs(user, mode.instrument, vid)
      sid0 <- recordStepAs(user, mode.instrument, aid0)
      aid1 <- recordAtomAs(user, mode.instrument, vid)
      sid1 <- recordStepAs(user, mode.instrument, aid1)
      _    <- addAtomEventAs(user, aid0, AtomStage.StartAtom)
      _    <- addStepEventAs(user, sid0, StepStage.StartStep)
      _    <- addStepEventAs(user, sid1, StepStage.StartStep)
      resA <- atomExecutionState(user, oid)
      resS <- stepExecutionState(user, oid)
    } yield {
      assertEquals(resA, List(AtomExecutionState.Abandoned, AtomExecutionState.Ongoing))
      assertEquals(resS, List(StepExecutionState.Abandoned, StepExecutionState.Ongoing))
    }
  }

  def addClientId(
    sid:         Step.Id,
    cid:         Client.Id,
    isDuplicate: Boolean
  ): IO[Unit] =
    expect(
      service,
      s"""
        mutation {
          addStepEvent(input: {
            stepId:    "$sid",
            stepStage: START_STEP,
            clientId:  "$cid"
          }) {
            event { clientId }
          }
        }
      """,
      Either.cond(
        !isDuplicate,
        json"""
          {
            "addStepEvent": {
              "event": {
                "clientId": $cid
              }
            }
          }
        """,
        List(s"An event with client id '$cid' has already been added.")
      )
    )

  test("addStepEvent - client id"):
    val cid  = Client.Id.parse("c-530c979f-de98-472f-9c23-a3442f2a9f7f")

    recordStep(ObservingModeType.GmosNorthLongSlit, service).flatMap: (_, sid) =>
      addClientId(sid, cid.get, isDuplicate = false)

  test("addStepEvent - duplicate client id"):
    val cid = Client.Id.parse("c-b7044cd8-38b5-4592-8d99-91d2c512041d")

    recordStep(ObservingModeType.GmosNorthLongSlit, service).flatMap: (_, sid) =>
      addClientId(sid, cid.get, isDuplicate = false) *>
      addClientId(sid, cid.get, isDuplicate = true)
}
