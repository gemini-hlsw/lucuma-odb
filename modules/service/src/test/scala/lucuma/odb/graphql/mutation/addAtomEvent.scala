// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.AtomStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.string.*
import lucuma.core.util.IdempotencyKey
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.StepExecutionState

class addAtomEvent extends OdbSuite with ExecutionState with query.ExecutionTestSupportForGmos:

  case class Setup(
    oid: Observation.Id,
    vid: Visit.Id,
    aids: List[Atom.Id],
    sids: Map[Atom.Id, List[Step.Id]]
  ):
    def aid0: Atom.Id = aids(0)
    def aid1: Atom.Id = aids(1)
    def aid2: Atom.Id = aids(2)
    def sid0: Step.Id = sids(aid0)(0)
    def sid1: Step.Id = sids(aid0)(1)

  private def setup(
    mode: ObservingModeType,
    user: User
  ):IO[Setup] =
    for
      pid  <- createProgramAs(user)
      tid  <- createTargetWithProfileAs(user, pid)
      oid  <- createObservationAs(user, pid, mode.some, tid)
      vid  <- recordVisitAs(user, mode.instrument, oid)
      aids <- selectGmosNorthScienceAtomIds(oid)
      sids <- selectGmosNorthScienceStepIds(oid)
    yield Setup(oid, vid, aids, sids)

  private def addAtomEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    (Atom.Id, Visit.Id) => String,
    expected: (Observation.Id, Atom.Id, Visit.Id) => Either[String, Json]
  ): IO[Unit] =
    for
      ids <- setup(mode, user)
      _   <- expect(user, query(ids.aid0, ids.vid), expected(ids.oid, ids.aid0, ids.vid).leftMap(s => List(s)))
    yield ()

  private def addAtomEventQuery(
    aid:   Atom.Id,
    vid:   Visit.Id,
    stage: AtomStage
  ): String =
    s"""
      mutation {
        addAtomEvent(input: {
          atomId:    "$aid",
          visitId:   "$vid",
          atomStage: ${stage.tag.toScreamingSnakeCase}
        }) {
          event {
            atom {
              id
              visit { id }
            }
            atomStage
            observation { id }
          }
        }
      }
    """

  test("addAtomEvent"):
    addAtomEventTest(
      ObservingModeType.GmosNorthLongSlit,
      serviceUser,
      (aid, vid) => addAtomEventQuery(aid, vid, AtomStage.StartAtom),
      (oid, aid, vid) => json"""
      {
        "addAtomEvent": {
          "event": {
            "atom": {
               "id": $aid,
               "visit": { "id" : $vid }
            },
            "atomStage": "START_ATOM",
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  test("addAtomEvent - unknown atom"):
    addAtomEventTest(
      ObservingModeType.GmosNorthLongSlit,
      serviceUser,
      (_, vid) => addAtomEventQuery(Atom.Id.parse("a-cfebc981-db7e-4c35-964d-6b19aa5ed2d7").get, vid, AtomStage.StartAtom),
      (_, _, _) => s"Atom 'a-cfebc981-db7e-4c35-964d-6b19aa5ed2d7' not found".asLeft
    )

  test("addAtomEvent - abandon atom"):
    val user = serviceUser

    import AtomExecutionState.*

    for
      ids <- setup(ObservingModeType.GmosNorthLongSlit, user)
      _   <- addAtomEventAs(user, ids.aid0, ids.vid, AtomStage.StartAtom) // 0 -> Ongoing
      _   <- addAtomEventAs(user, ids.aid1, ids.vid, AtomStage.StartAtom) // 0 -> Completed, 1 -> Ongoing
      _   <- addAtomEventAs(user, ids.aid1, ids.vid, AtomStage.EndAtom)   // 0 -> Completed, 1 -> Completed
      _   <- addAtomEventAs(user, ids.aid2, ids.vid, AtomStage.StartAtom) // 0 -> Completed, 1 -> Completed, 2 -> Ongoing
      res <- atomExecutionState(user, ids.oid)
    yield assertEquals(res, List(Abandoned, Completed, Ongoing))

  test("start step starts atom without atom events"):
    import AtomExecutionState.*
    for
      ids <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      _   <- addStepEventAs(serviceUser, ids.sid0, ids.vid, StepStage.StartStep)
      res <- atomExecutionState(serviceUser, ids.oid)
    yield assertEquals(res, List(Ongoing))

  test("start step re-starts completed atom"):
    val user = serviceUser

    import AtomExecutionState.*

    for
      ids  <- setup(ObservingModeType.GmosNorthLongSlit, user)
      _    <- addStepEventAs(user, ids.sid0, ids.vid, StepStage.StartStep)
      _    <- addAtomEventAs(user, ids.aid0, ids.vid, AtomStage.EndAtom)
      res0 <- atomExecutionState(user, ids.oid)
      _    <- addStepEventAs(user, ids.sid1, ids.vid, StepStage.StartStep)
      res1 <- atomExecutionState(user, ids.oid)
    yield
      assertEquals(res0, List(Completed))
      assertEquals(res1, List(Ongoing))

  test("terminal sequence events complete atoms"):
    val user = serviceUser

    import AtomExecutionState.*

    for
      ids <- setup(ObservingModeType.GmosNorthLongSlit, user)
      _   <- addStepEventAs(user, ids.sid0, ids.vid, StepStage.StartStep)
      _   <- addSequenceEventAs(user, ids.vid, SequenceCommand.Stop)
      res <- atomExecutionState(user, ids.oid)
    yield
      assertEquals(res, List(Abandoned))

  test("addAtomEvent - abandon step"):
    val user = serviceUser

    for
      ids  <- setup(ObservingModeType.GmosNorthLongSlit, user)
      _    <- addAtomEventAs(user, ids.aid0, ids.vid, AtomStage.StartAtom)
      _    <- addStepEventAs(user, ids.sid0, ids.vid, StepStage.StartStep)
      _    <- addAtomEventAs(user, ids.aid1, ids.vid, AtomStage.StartAtom)
      resA <- atomExecutionState(user, ids.oid)
      resS <- stepExecutionState(user, ids.oid)
    yield
      assertEquals(resA, List(AtomExecutionState.Abandoned, AtomExecutionState.Ongoing))
      assertEquals(resS, List(StepExecutionState.Abandoned))

  test("addAtomEvent - idempotency key"):
    val user = serviceUser
    val idm  = IdempotencyKey.FromString.getOption("530c979f-de98-472f-9c23-a3442f2a9f7f")

    for
      ids <- setup(ObservingModeType.GmosNorthLongSlit, user)
      evt <- addAtomEventAs(user, ids.aid0, ids.vid, AtomStage.StartAtom, idempotencyKey = idm)
    yield assertEquals(evt.idempotencyKey, idm)

  test("addAtomEvent - duplicate idempotency key"):
    val user = serviceUser
    val idm  = IdempotencyKey.FromString.getOption("b7044cd8-38b5-4592-8d99-91d2c512041d")

    for
      ids <- setup(ObservingModeType.GmosNorthLongSlit, user)
      evt <- addAtomEventAs(user, ids.aid0, ids.vid, AtomStage.StartAtom, idempotencyKey = idm)
      _   <- expect(user,
        s"""
          mutation {
            addAtomEvent(input: {
              atomId: "${ids.aid0}"
              visitId: "${ids.vid}"
              atomStage: START_ATOM
              idempotencyKey: "${idm.get}"
            }) {
              event { id }
            }
          }
        """,
        json"""
          {
            "addAtomEvent": {
              "event": { "id": ${evt.id.asJson} }
            }
          }
        """.asRight
      )
    yield ()