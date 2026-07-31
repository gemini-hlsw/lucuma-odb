// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepStage
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp

import java.time.Instant
import java.util.UUID

class addEvents extends OdbSuite with query.ExecutionTestSupportForGmos:

  val mode: ObservingModeType = ObservingModeType.GmosNorthLongSlit

  private def nowMinus(seconds: Long): Timestamp =
    Timestamp.unsafeFromInstantTruncated(Instant.now().minusSeconds(seconds))

  private def newKey: String = UUID.randomUUID.toString

  // Records an observation with a visit, a step, and a dataset to hang events on.
  private def setup(user: User, file: String): IO[(Observation.Id, Visit.Id, Step.Id, Dataset.Id)] =
    for
      pid  <- createProgramAs(user)
      tid  <- createTargetWithProfileAs(user, pid)
      oid  <- createObservationAs(user, pid, mode.some, tid)
      vid  <- recordVisitAs(user, oid)
      sids <- scienceSequenceIds(user, oid)
      sid   = sids.head._2.apply(0)
      _    <- addStepEventAs(user, sid, vid, StepStage.StartStep) // create the step execution record
      did  <- recordDatasetAs(user, sid, vid, file)
    yield (oid, vid, sid, did)

  private def stepEvent(sid: Step.Id, vid: Visit.Id, stage: String, t: Timestamp, key: String): String =
    s"""{ step: { stepId: "$sid", visitId: "$vid", stepStage: $stage, clientTime: "${t.isoFormat}", idempotencyKey: "$key" } }"""

  private def datasetEvent(did: Dataset.Id, stage: String, t: Timestamp, key: String): String =
    s"""{ dataset: { datasetId: "$did", datasetStage: $stage, clientTime: "${t.isoFormat}", idempotencyKey: "$key" } }"""

  private def addEventsQuery(events: List[String]): String =
    s"""
      mutation {
        addEvents(input: { events: [ ${events.mkString(", ")} ] }) {
          events {
            eventType
            effectiveTime
          }
        }
      }
    """

  test("addEvents - a mixed batch is recorded in order"):
    val ts = List(nowMinus(240), nowMinus(180), nowMinus(120), nowMinus(60))
    setup(serviceUser, "N18630101S1001.fits").flatMap: (_, vid, sid, did) =>
      val q = addEventsQuery(List(
        stepEvent(sid, vid, "START_STEP", ts(0), newKey),
        datasetEvent(did, "START_EXPOSE", ts(1), newKey),
        datasetEvent(did, "END_WRITE", ts(2), newKey),
        stepEvent(sid, vid, "END_STEP", ts(3), newKey)
      ))
      query(serviceUser, q).map: json =>
        val events = json.hcursor.downFields("addEvents", "events").values.toList.flatten
        val types  = events.map(_.hcursor.downField("eventType").require[String])
        val times  = events.map(_.hcursor.downField("effectiveTime").require[Timestamp])
        assertEquals(types, List("STEP", "DATASET", "DATASET", "STEP"))
        assertEquals(times, ts)

  test("addEvents - a missing clientTime is rejected"):
    setup(serviceUser, "N18630101S1002.fits").flatMap: (_, vid, sid, _) =>
      val q =
        s"""
          mutation {
            addEvents(input: { events: [
              { step: { stepId: "$sid", visitId: "$vid", stepStage: START_STEP, idempotencyKey: "$newKey" } }
            ] }) { events { eventType } }
          }
        """
      expect(serviceUser, q, List("Argument 'input.events' is invalid: at index 0: Each event in a batch must supply a 'clientTime'.").asLeft)

  test("addEvents - a missing idempotencyKey is rejected"):
    setup(serviceUser, "N18630101S1003.fits").flatMap: (_, vid, sid, _) =>
      val q =
        s"""
          mutation {
            addEvents(input: { events: [
              { step: { stepId: "$sid", visitId: "$vid", stepStage: START_STEP, clientTime: "${nowMinus(60).isoFormat}" } }
            ] }) { events { eventType } }
          }
        """
      expect(serviceUser, q, List("Argument 'input.events' is invalid: at index 0: Each event in a batch must supply an 'idempotencyKey'.").asLeft)

  test("addEvents - duplicate idempotency keys within a batch are rejected"):
    val key = newKey
    setup(serviceUser, "N18630101S1004.fits").flatMap: (_, vid, sid, _) =>
      val q = addEventsQuery(List(
        stepEvent(sid, vid, "START_STEP", nowMinus(120), key),
        stepEvent(sid, vid, "END_STEP", nowMinus(60), key)
      ))
      expect(serviceUser, q, List("Argument 'input' is invalid: Idempotency keys within a batch must be distinct.").asLeft)

  test("addEvents - an empty batch is rejected"):
    val q =
      s"""
        mutation {
          addEvents(input: { events: [] }) { events { eventType } }
        }
      """
    expect(serviceUser, q, List("Argument 'input' is invalid: An 'addEvents' batch must contain at least one event.").asLeft)

  test("addEvents - events from two observations are rejected"):
    for
      a  <- setup(serviceUser, "N18630101S1005.fits")
      b  <- setup(serviceUser, "N18630101S1006.fits")
      (_, vidA, sidA, _) = a
      (_, vidB, sidB, _) = b
      q   = addEventsQuery(List(
              stepEvent(sidA, vidA, "START_STEP", nowMinus(120), newKey),
              stepEvent(sidB, vidB, "START_STEP", nowMinus(60), newKey)
            ))
      _  <- expect(serviceUser, q, List("All events in a batch must belong to the same observation.").asLeft)
    yield ()

  test("addEvents - retrying a batch is idempotent"):
    val events = (sid: Step.Id, vid: Visit.Id, did: Dataset.Id) => List(
      stepEvent(sid, vid, "START_STEP", nowMinus(120), newKey),
      datasetEvent(did, "START_EXPOSE", nowMinus(60), newKey)
    )

    def eventIds(q: String): IO[List[ExecutionEvent.Id]] =
      query(serviceUser, q).map: json =>
        json.hcursor
          .downFields("addEvents", "events")
          .values.toList.flatten
          .traverse(_.hcursor.downField("id").as[ExecutionEvent.Id])
          .fold(f => throw new RuntimeException(f.message), identity)

    setup(serviceUser, "N18630101S1007.fits").flatMap: (_, vid, sid, did) =>
      val es = events(sid, vid, did)
      // Same events (same keys) must yield the same ids on a resend.
      val q  =
        s"""
          mutation {
            addEvents(input: { events: [ ${es.mkString(", ")} ] }) {
              events { id }
            }
          }
        """
      for
        first  <- eventIds(q)
        second <- eventIds(q)
      yield assertEquals(first, second)
