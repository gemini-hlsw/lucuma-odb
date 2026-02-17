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
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.util.IdempotencyKey
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.StepExecutionState


class addStepEvent extends OdbSuite with ExecutionState with query.ExecutionTestSupportForGmos:

  case class Setup(
    oid:  Observation.Id,
    vid:  Visit.Id,
    aids: List[Atom.Id],
    sids: Map[Atom.Id, List[Step.Id]]
  ):
    def aid0: Atom.Id = aids(0)
    def aid1: Atom.Id = aids(1)
    def aid2: Atom.Id = aids(2)
    def sid0: Step.Id = sids(aid0)(0)
    def sid1: Step.Id = sids(aid0)(1)
    def sid2: Step.Id = sids(aid0)(2)

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

  private def addStepEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Setup => String,
    expected: Setup => Either[String, Json]
  ): IO[Unit] =
    for
      su <- setup(mode, user)
      _  <- expect(user, query(su), expected(su).leftMap(s => List(s)))
    yield ()

  test("addStepEvent"):
    def query(su: Setup): String =
      s"""
        mutation {
          addStepEvent(input: {
            stepId:    "${su.sid0}"
            visitId:   "${su.vid}"
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
      serviceUser,
      su => query(su),
      su => json"""
      {
        "addStepEvent": {
          "event": {
            "step": {
               "id": ${su.sid0}
            },
            "stepStage": "START_STEP",
            "observation": {
              "id": ${su.oid}
            }
          }
        }
      }
      """.asRight
    )

  test("addStepEvent - unknown step"):
    def query(vid: Visit.Id): String =
      s"""
        mutation {
          addStepEvent(input: {
            stepId:    "s-cfebc981-db7e-4c35-964d-6b19aa5ed2d7"
            visitId:   "$vid"
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
      serviceUser,
      su => query(su.vid),
      _  => s"Step 's-cfebc981-db7e-4c35-964d-6b19aa5ed2d7' not found".asLeft
    )

  test("addStepEvent - once terminal, state doesn't change"):
    import StepExecutionState.*

    for
      su  <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      _   <- addAtomEventAs(serviceUser, su.aid0, su.vid, AtomStage.StartAtom)
      _   <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.StartStep)
      _   <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.Abort)
      _   <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.EndStep) // won't change state
      res <- stepExecutionState(serviceUser, su.oid)
    yield assertEquals(res, List(Aborted))

  test("addStepEvent - abandon step"):
    import StepExecutionState.*

    for
      su   <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      _    <- addAtomEventAs(serviceUser, su.aid0, su.vid, AtomStage.StartAtom)
      _    <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.StartStep)
      _    <- addStepEventAs(serviceUser, su.sid1, su.vid, StepStage.StartStep)
      _    <- addStepEventAs(serviceUser, su.sid1, su.vid, StepStage.EndStep)
      _    <- addStepEventAs(serviceUser, su.sid2, su.vid, StepStage.StartStep)
      res  <- stepExecutionState(serviceUser, su.oid)
    yield assertEquals(res, List(Abandoned, Completed, Ongoing))

  test("addStepEvent - abandon atom"):
    for
      su   <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      sid1  = su.sids(su.aid1)(0)
      _    <- addAtomEventAs(serviceUser, su.aid0, su.vid, AtomStage.StartAtom)
      _    <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.StartStep)
      _    <- addStepEventAs(serviceUser, sid1, su.vid, StepStage.StartStep)
      resA <- atomExecutionState(serviceUser, su.oid)
      resS <- stepExecutionState(serviceUser, su.oid)
    yield
      assertEquals(resA, List(AtomExecutionState.Completed, AtomExecutionState.Ongoing))
      assertEquals(resS, List(StepExecutionState.Abandoned, StepExecutionState.Ongoing))

  def addWithIdempotencyKey(
    sid: Step.Id,
    vid: Visit.Id,
    idm: Option[IdempotencyKey] = None
  ): IO[(ExecutionEvent.Id, Option[IdempotencyKey])] =
    query(
      serviceUser,
      s"""
        mutation {
          addStepEvent(input: {
            stepId:    "$sid"
            visitId:   "$vid"
            stepStage: START_STEP
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
      val cur = js.hcursor.downFields("addStepEvent", "event")
      (for
        e <- cur.downField("id").as[ExecutionEvent.Id]
        n <- cur.downField("idempotencyKey").as[Option[IdempotencyKey]]
      yield (e, n)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  test("addStepEvent - idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b9bac66c-4e12-4b1d-b646-47c2c3a97792")

    setup(ObservingModeType.GmosNorthLongSlit, serviceUser).flatMap: su =>
      assertIO(addWithIdempotencyKey(su.sid0, su.vid, idm = idm).map(_._2), idm)

  test("addStepEvent - duplicate idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b7044cd8-38b5-4592-8d99-91d2c512041d")

    setup(ObservingModeType.GmosNorthLongSlit, serviceUser).flatMap: su =>
      addWithIdempotencyKey(su.sid0, su.vid, idm = idm).flatMap: (eid, _) =>
        assertIO(addWithIdempotencyKey(su.sid0, su.vid, idm = idm).map(_._1), eid)