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
import lucuma.core.model.Client
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.syntax.string.*
import lucuma.core.util.IdempotencyKey
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.StepExecutionState

class addAtomEvent extends OdbSuite with ExecutionState:

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordAtom(
    mode: ObservingModeType,
    user: User
  ):IO[(Observation.Id, Atom.Id)] =
    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
    yield (oid, aid)

  private def addAtomEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Atom.Id => String,
    expected: (Observation.Id, Atom.Id) => Either[String, Json]
  ): IO[Unit] =
    for
      ids <- recordAtom(mode, user)
      (oid, aid) = ids
      _   <- expect(user, query(aid), expected(oid, aid).leftMap(s => List(s)))
    yield ()

  private def addAtomEventQuery(
    aid:   Atom.Id,
    stage: AtomStage
  ): String =
    s"""
      mutation {
        addAtomEvent(input: {
          atomId:    "$aid",
          atomStage: ${stage.tag.toScreamingSnakeCase}
        }) {
          event {
            atom {
              id
            }
            atomStage
            observation {
              id
            }
          }
        }
      }
    """

  test("addAtomEvent"):
    addAtomEventTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      aid => addAtomEventQuery(aid, AtomStage.StartAtom),
      (oid, aid) => json"""
      {
        "addAtomEvent": {
          "event": {
            "atom": {
               "id": $aid
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
      service,
      _ => addAtomEventQuery(Atom.Id.parse("a-cfebc981-db7e-4c35-964d-6b19aa5ed2d7").get, AtomStage.StartAtom),
      (_, _) => s"Atom 'a-cfebc981-db7e-4c35-964d-6b19aa5ed2d7' not found".asLeft
    )

  test("addAtomEvent - abandon atom"):
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit

    import AtomExecutionState.*

    for
      pid  <- createProgramAs(user)
      oid  <- createObservationAs(user, pid, mode.some)
      vid  <- recordVisitAs(user, mode.instrument, oid)
      aid0 <- recordAtomAs(user, mode.instrument, vid)
      aid1 <- recordAtomAs(user, mode.instrument, vid)
      aid2 <- recordAtomAs(user, mode.instrument, vid)
      _    <- addAtomEventAs(user, aid0, AtomStage.StartAtom) // 0 -> Ongoing
      _    <- addAtomEventAs(user, aid1, AtomStage.StartAtom) // 0 -> Completed, 1 -> Ongoing
      _    <- addAtomEventAs(user, aid1, AtomStage.EndAtom)   // 0 -> Completed, 1 -> Completed
      _    <- addAtomEventAs(user, aid2, AtomStage.StartAtom) // 0 -> Completed, 1 -> Completed, 2 -> Ongoing
      res  <- atomExecutionState(user, oid)
    yield assertEquals(res, List(Completed, Completed, Ongoing))

  test("start step starts atom without atom events"):
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit

    import AtomExecutionState.*

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
      _   <- addStepEventAs(user, sid, StepStage.StartStep)
      res <- atomExecutionState(user, oid)
    yield assertEquals(res, List(Ongoing))

  test("start step re-starts completed atom"):
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit

    import AtomExecutionState.*

    for
      pid  <- createProgramAs(user)
      oid  <- createObservationAs(user, pid, mode.some)
      vid  <- recordVisitAs(user, mode.instrument, oid)
      aid  <- recordAtomAs(user, mode.instrument, vid)
      sid0 <- recordStepAs(user, mode.instrument, aid)
      _    <- addStepEventAs(user, sid0, StepStage.StartStep)
      _    <- addAtomEventAs(user, aid, AtomStage.EndAtom)
      res0 <- atomExecutionState(user, oid)
      sid1 <- recordStepAs(user, mode.instrument, aid)
      _    <- addStepEventAs(user, sid1, StepStage.StartStep)
      res1 <- atomExecutionState(user, oid)
    yield
      assertEquals(res0, List(Completed))
      assertEquals(res1, List(Ongoing))

  test("terminal sequence events complete atoms"):
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit

    import AtomExecutionState.*

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
      _   <- addStepEventAs(user, sid, StepStage.StartStep)
      _   <- addSequenceEventAs(user, vid, SequenceCommand.Stop)
      res <- atomExecutionState(user, oid)
    yield
      assertEquals(res, List(Completed))

  test("addAtomEvent - abandon step"):
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit

    for
      pid  <- createProgramAs(user)
      oid  <- createObservationAs(user, pid, mode.some)
      vid  <- recordVisitAs(user, mode.instrument, oid)
      aid0 <- recordAtomAs(user, mode.instrument, vid)
      sid0 <- recordStepAs(user, mode.instrument, aid0)
      aid1 <- recordAtomAs(user, mode.instrument, vid)
      _    <- addAtomEventAs(user, aid0, AtomStage.StartAtom)
      _    <- addStepEventAs(user, sid0, StepStage.StartStep)
      _    <- addAtomEventAs(user, aid1, AtomStage.StartAtom)
      resA <- atomExecutionState(user, oid)
      resS <- stepExecutionState(user, oid)
    yield
      assertEquals(resA, List(AtomExecutionState.Completed, AtomExecutionState.Ongoing))
      assertEquals(resS, List(StepExecutionState.Abandoned))

  test("addAtomEvent - client id"):
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit
    val cid  = Client.Id.parse("c-530c979f-de98-472f-9c23-a3442f2a9f7f")

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
      evt <- addAtomEventAs(user, aid, AtomStage.StartAtom, clientId = cid)
    yield assertEquals(evt.idempotencyKey.map(k => Client.Id.fromUuid(k.value)), cid)

  test("addAtomEvent - idempotency key"):
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit
    val idm  = IdempotencyKey.FromString.getOption("530c979f-de98-472f-9c23-a3442f2a9f7f")

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
      evt <- addAtomEventAs(user, aid, AtomStage.StartAtom, idempotencyKey = idm)
    yield assertEquals(evt.idempotencyKey, idm)

  test("addAtomEvent - duplicate idempotency key"):
    val user = service
    val mode = ObservingModeType.GmosNorthLongSlit
    val idm  = IdempotencyKey.FromString.getOption("b7044cd8-38b5-4592-8d99-91d2c512041d")

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
      evt <- addAtomEventAs(user, aid, AtomStage.StartAtom, idempotencyKey = idm)
      _   <- expect(user,
        s"""
          mutation {
            addAtomEvent(input: {
              atomId: "$aid"
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