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
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.StepExecutionState

class addSequenceEvent extends OdbSuite with ExecutionState {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordVisit(
    mode: ObservingModeType,
    user: User
  ):IO[(Observation.Id, Visit.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
    } yield (oid, vid)

  private def addSequenceEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Visit.Id => String,
    expected: (Observation.Id, Visit.Id) => Either[String, Json]
  ): IO[Unit] = {
    for {
      ids <- recordVisit(mode, user)
      (oid, vid) = ids
      _   <- expect(user, query(vid), expected(oid, vid).leftMap(s => List(s)))
    } yield ()
}

  test("addSequenceEvent") {
    def query(vid: Visit.Id): String =
      s"""
        mutation {
          addSequenceEvent(input: {
            visitId: "$vid",
            command: START
          }) {
            event {
              visit {
                id
              }
              observation {
                id
              }
            }
          }
        }
      """

    addSequenceEventTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      vid => query(vid),
      (oid, vid) => json"""
      {
        "addSequenceEvent": {
          "event": {
            "visit": {
              "id": $vid
            },
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  }

  test("addSequenceEvent - unknown visit") {
    def query: String =
      s"""
        mutation {
          addSequenceEvent(input: {
            visitId: "v-42",
            command: START
          }) {
            event {
              visit {
                id
              }
              observation {
                id
              }
            }
          }
        }
      """

    addSequenceEventTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      _ => query,
      (_, _) => s"Visit 'v-42' not found".asLeft
    )

  }

  test("addSequenceEvent - abandon atoms and steps") {
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
      _    <- addStepEventAs(user, sid0, StepStage.EndStep)
      _    <- addAtomEventAs(user, aid0, AtomStage.EndAtom)
      _    <- addAtomEventAs(user, aid0, AtomStage.StartAtom)
      _    <- addStepEventAs(user, sid1, StepStage.StartStep)
      _    <- addSequenceEventAs(user, vid, SequenceCommand.Abort)
      resA <- atomExecutionState(user, oid)
      resS <- stepExecutionState(user, oid)
    } yield {
      assertEquals(resA, List(AtomExecutionState.Completed, AtomExecutionState.Abandoned))
      assertEquals(resS, List(StepExecutionState.Completed, StepExecutionState.Abandoned))
    }
  }

}
