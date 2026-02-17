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
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.util.IdempotencyKey
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.StepExecutionState

class addSequenceEvent extends OdbSuite with ExecutionState with query.ExecutionTestSupportForGmos:

  private def recordVisit(
    mode: ObservingModeType,
    user: User
  ):IO[(Observation.Id, Visit.Id)] =
    for
      pid <- createProgramAs(user)
      tid  <- createTargetWithProfileAs(user, pid)
      oid <- createObservationAs(user, pid, mode.some, tid)
      vid <- recordVisitAs(user, mode.instrument, oid)
    yield (oid, vid)

  private def addSequenceEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Visit.Id => String,
    expected: (Observation.Id, Visit.Id) => Either[String, Json]
  ): IO[Unit] =
    for
      ids <- recordVisit(mode, user)
      (oid, vid) = ids
      _   <- expect(user, query(vid), expected(oid, vid).leftMap(s => List(s)))
    yield ()

  test("addSequenceEvent"):
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
      serviceUser,
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

  test("addSequenceEvent - unknown visit"):
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
      serviceUser,
      _ => query,
      (_, _) => s"Visit 'v-42' not found".asLeft
    )

  test("addSequenceEvent - abandon atoms and steps"):
    val user = serviceUser
    val mode = ObservingModeType.GmosNorthLongSlit

    for
      pid  <- createProgramAs(user)
      tid  <- createTargetWithProfileAs(user, pid)
      oid  <- createObservationAs(user, pid, mode.some, tid)
      vid  <- recordVisitAs(user, mode.instrument, oid)
      aids <- selectGmosNorthScienceAtomIds(oid)
      sids <- selectGmosNorthScienceStepIds(oid)
      aid0  = aids(0)
      sid0  = sids(aid0)(0)
      aid1  = aids(1)
      sid1  = sids(aid1)(0)
      _    <- addAtomEventAs(user, aid0, vid, AtomStage.StartAtom)
      _    <- addStepEventAs(user, sid0, vid, StepStage.StartStep)
      _    <- addStepEventAs(user, sid0, vid, StepStage.EndStep)
      _    <- addAtomEventAs(user, aid0, vid, AtomStage.EndAtom)
      _    <- addAtomEventAs(user, aid0, vid, AtomStage.StartAtom)
      _    <- addStepEventAs(user, sid1, vid, StepStage.StartStep)
      _    <- addSequenceEventAs(user, vid, SequenceCommand.Abort)
      resA <- atomExecutionState(user, oid)
      resS <- stepExecutionState(user, oid)
    yield
      assertEquals(resA, List(AtomExecutionState.Completed, AtomExecutionState.Completed))
      assertEquals(resS, List(StepExecutionState.Completed, StepExecutionState.Abandoned))

  def addWithIdempotencyKey(
    vid: Visit.Id,
    idm: Option[IdempotencyKey] = None
  ): IO[(ExecutionEvent.Id, Option[IdempotencyKey])] =
    query(
      serviceUser,
      s"""
        mutation {
          addSequenceEvent(input: {
            visitId: "$vid"
            command: START
            ${idm.fold("")(idm => s"idempotencyKey: \"$idm\"")}
          }) {
            event {
              id
              idempotencyKey
            }
          }
        }
      """
    ).flatMap: js =>
      val cur = js.hcursor.downFields("addSequenceEvent", "event")
      (for
        e <- cur.downField("id").as[ExecutionEvent.Id]
        n <- cur.downField("idempotencyKey").as[Option[IdempotencyKey]]
      yield (e, n)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  test("addSequenceEvent - idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b9bac66c-4e12-4b1d-b646-47c2c3a97792")

    recordVisit(ObservingModeType.GmosNorthLongSlit, serviceUser).flatMap: (_, vid) =>
      assertIO(addWithIdempotencyKey(vid, idm = idm).map(_._2), idm)

  test("addSequenceEvent - duplicate idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b7044cd8-38b5-4592-8d99-91d2c512041d")

    recordVisit(ObservingModeType.GmosNorthLongSlit, serviceUser).flatMap: (_, vid) =>
      addWithIdempotencyKey(vid, idm = idm).flatMap: (eid, _) =>
        assertIO(addWithIdempotencyKey(vid, idm = idm).map(_._1), eid)