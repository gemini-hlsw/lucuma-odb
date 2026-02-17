// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
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

class addAtomEvent extends OdbSuite with ExecutionState with query.ExecutionTestSupportForGmos:

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
    query:    Setup => String,
    expected: Setup => Either[String, Json]
  ): IO[Unit] =
    for
      su <- setup(mode, user)
      _  <- expect(user, query(su), expected(su).leftMap(s => List(s)))
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
      su => addAtomEventQuery(su.aid0, su.vid, AtomStage.StartAtom),
      su => json"""
      {
        "addAtomEvent": {
          "event": {
            "atom": {
               "id": ${su.aid0},
               "visit": { "id" : ${su.vid} }
            },
            "atomStage": "START_ATOM",
            "observation": {
              "id": ${su.oid}
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
      su => addAtomEventQuery(Atom.Id.parse("a-cfebc981-db7e-4c35-964d-6b19aa5ed2d7").get, su.vid, AtomStage.StartAtom),
      _  => s"Atom 'a-cfebc981-db7e-4c35-964d-6b19aa5ed2d7' not found".asLeft
    )

  test("addAtomEvent - no impact on completion state"):
    import AtomExecutionState.*

    for
      su  <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      _   <- addAtomEventAs(serviceUser, su.aid0, su.vid, AtomStage.StartAtom)
      _   <- addAtomEventAs(serviceUser, su.aid1, su.vid, AtomStage.StartAtom)
      _   <- addAtomEventAs(serviceUser, su.aid1, su.vid, AtomStage.EndAtom)
      _   <- addAtomEventAs(serviceUser, su.aid2, su.vid, AtomStage.StartAtom)
      res <- atomExecutionState(serviceUser, su.oid)
    yield assertEquals(res, List(NotStarted, NotStarted, NotStarted))

  test("start step starts atom without atom events"):
    import AtomExecutionState.*
    for
      su  <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      _   <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.StartStep)
      res <- atomExecutionState(serviceUser, su.oid)
    yield assertEquals(res, List(Ongoing))

  test("completing all steps completes the atom"):
    import AtomExecutionState.*

    for
      su   <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      _    <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.StartStep)
      res0 <- atomExecutionState(serviceUser, su.oid)
      _    <- su.sids(su.aid0).traverse(sid => addStepEventAs(serviceUser, sid, su.vid, StepStage.EndStep))
      res1 <- atomExecutionState(serviceUser, su.oid)
    yield
      assertEquals(res0, List(Ongoing))
      assertEquals(res1, List(Completed))

  test("starting a step in a different atom completes the ongoing atom"):
    import AtomExecutionState.*

    for
      su   <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      _    <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.StartStep)
      res0 <- atomExecutionState(serviceUser, su.oid)
      _    <- addStepEventAs(serviceUser, su.sids(su.aid1)(0), su.vid, StepStage.StartStep)
      res1 <- atomExecutionState(serviceUser, su.oid)
    yield
      assertEquals(res0, List(Ongoing))
      assertEquals(res1, List(Completed, Ongoing))

  test("terminal sequence events complete atoms"):
    import AtomExecutionState.*

    for
      su  <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      _   <- addStepEventAs(serviceUser, su.sid0, su.vid, StepStage.StartStep)
      _   <- addSequenceEventAs(serviceUser, su.vid, SequenceCommand.Stop)
      res <- atomExecutionState(serviceUser, su.oid)
    yield
      assertEquals(res, List(Completed))

  test("addAtomEvent - idempotency key"):
    val idm  = IdempotencyKey.FromString.getOption("530c979f-de98-472f-9c23-a3442f2a9f7f")

    for
      su  <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      evt <- addAtomEventAs(serviceUser, su.aid0, su.vid, AtomStage.StartAtom, idempotencyKey = idm)
    yield assertEquals(evt.idempotencyKey, idm)

  test("addAtomEvent - duplicate idempotency key"):
    val idm  = IdempotencyKey.FromString.getOption("b7044cd8-38b5-4592-8d99-91d2c512041d")

    for
      su  <- setup(ObservingModeType.GmosNorthLongSlit, serviceUser)
      evt <- addAtomEventAs(serviceUser, su.aid0, su.vid, AtomStage.StartAtom, idempotencyKey = idm)
      _   <- expect(serviceUser,
        s"""
          mutation {
            addAtomEvent(input: {
              atomId: "${su.aid0}"
              visitId: "${su.vid}"
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