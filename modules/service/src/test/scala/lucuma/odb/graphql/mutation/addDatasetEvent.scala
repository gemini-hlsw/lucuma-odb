// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepStage
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval

import java.time.Instant


class addDatasetEvent extends OdbSuite with query.ExecutionTestSupportForGmos:

  val mode: ObservingModeType = ObservingModeType.GmosNorthLongSlit

  private def recordDataset(
    mode: ObservingModeType,
    user: User,
    file: String
  ):IO[(Observation.Id, Dataset.Id)] =
    for
      pid  <- createProgramAs(user)
      tid  <- createTargetWithProfileAs(user, pid)
      oid  <- createObservationAs(user, pid, mode.some, tid)
      vid  <- recordVisitAs(user, oid)
      sids <- scienceSequenceIds(user, oid)
      aid0  = sids.head._1
      sid0  = sids.head._2.apply(0)
      _    <- addStepEventAs(user, sid0, vid, StepStage.EndStep)
      did  <- recordDatasetAs(user, sid0, vid, file)
    yield (oid, did)

  private def addDatasetEventTest(
    mode:     ObservingModeType,
    user:     User,
    file:     String,
    query:    Dataset.Id => String,
    expected: (Observation.Id, Dataset.Id) => Either[String, Json]
  ): IO[Unit] =
    for
      ids <- recordDataset(mode, user, file)
      (oid, did) = ids
      _   <- expect(user, query(did), expected(oid, did).leftMap(s => List(s)))
    yield ()

  test("addDatasetEvent"):
    def query(did: Dataset.Id): String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: "$did",
            datasetStage: START_WRITE
          }) {
            event {
              datasetStage
              dataset {
                id
              }
              observation {
                id
              }
            }
          }
        }
      """

    addDatasetEventTest(
      mode,
      serviceUser,
      "N18630101S0001.fits",
      did => query(did),
      (oid, did) => json"""
      {
        "addDatasetEvent": {
          "event": {
            "datasetStage": "START_WRITE",
            "dataset": {
              "id": $did
            },
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  test("addDatasetEvent - with filename"):
    def query(did: Dataset.Id): String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: "$did",
            datasetStage: START_WRITE
          }) {
            event {
              datasetStage
              dataset {
                id
              }
              observation {
                id
              }
            }
          }
        }
      """

    addDatasetEventTest(
      mode,
      serviceUser,
      "N18630101S0002.fits",
      did => query(did),
      (oid, did) => json"""
      {
        "addDatasetEvent": {
          "event": {
            "datasetStage": "START_WRITE",
            "dataset": {
              "id": $did
            },
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  test("addDatasetEvent - unknown dataset"):
    val query: String =
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId: "d-1863",
            datasetStage: START_WRITE
          }) {
            event {
              dataset {
                id
              }
            }
          }
        }
      """

    addDatasetEventTest(
      mode,
      serviceUser,
      "N18630101S0003.fits",
      _ => query,
      (_, _) => s"Dataset 'd-1863' not found".asLeft
    )

  private def addEvent(did: Dataset.Id, stage: DatasetStage): IO[Timestamp] =
    query(
      serviceUser,
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId:    "$did",
            datasetStage: ${stage.tag.toUpperCase}
          }) {
            event {
              received
            }
          }
        }
      """
    ).map: json =>
      json.hcursor.downFields("addDatasetEvent", "event", "received").require[Timestamp]

  private def timestamps(did: Dataset.Id): IO[Option[TimestampInterval]] =
    query(
      serviceUser,
      s"""
        query {
          dataset(datasetId: "$did") {
            interval {
              start
              end
            }
          }
        }
      """
    ).map: json =>
      val d = json.hcursor.downFields("dataset", "interval").success.filter(!_.value.isNull)
      val s = d.flatMap(_.downField("start").require[Option[Timestamp]])
      val e = d.flatMap(_.downField("end").require[Option[Timestamp]])
      (s, e).mapN { (start, end) => TimestampInterval.between(start, end) }

  private def timeTest(file: String, stages: DatasetStage*): IO[Unit] =
    def expected(times: List[Timestamp]): (Option[Timestamp], Option[Timestamp]) =
      times.zip(stages).foldLeft((Option.empty[Timestamp], Option.empty[Timestamp])) { case ((start, end), (time, stage)) =>
        if (stage === DatasetStage.StartExpose) (time.some, none)
        else if ((stage === DatasetStage.EndWrite) && start.isDefined) (start, time.some)
        else (start, end)
      }

    for
      ids <- recordDataset(mode, serviceUser, file)
      (oid, did) = ids
      es  <- stages.toList.traverse(addEvent(did, _))
      ex   = expected(es).mapN { (s, e) => TimestampInterval.between(s, e) }
      ts  <- timestamps(did)
    yield assertEquals(ts, ex)

  test("addDatasetEvent - no start time"):
    timeTest("N18630101S0004.fits", DatasetStage.StartWrite)

  test("addDatasetEvent - start"):
    timeTest("N18630101S0005.fits", DatasetStage.StartExpose)

  test("addDatasetEvent - start, end"):
    timeTest("N18630101S0006.fits", DatasetStage.StartExpose, DatasetStage.EndWrite)

  test("addDatasetEvent - end, no start"):
    timeTest("N18630101S0007.fits", DatasetStage.EndWrite)

  test("addDatasetEvent - start, end, start"):
    timeTest("N18630101S0008.fits", DatasetStage.StartExpose, DatasetStage.EndWrite, DatasetStage.StartExpose)

  def addWithIdempotencyKey(
    did: Dataset.Id,
    idm: Option[IdempotencyKey] = None
  ): IO[(ExecutionEvent.Id, Option[IdempotencyKey])] =
      query(
        serviceUser,
        s"""
          mutation {
            addDatasetEvent(input: {
              datasetId: "$did"
              datasetStage: START_WRITE
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
        val cur = js.hcursor.downFields("addDatasetEvent", "event")
        (for
          e <- cur.downField("id").as[ExecutionEvent.Id]
          n <- cur.downField("idempotencyKey").as[Option[IdempotencyKey]]
        yield (e, n)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  test("addDatasetEvent - idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b9bac66c-4e12-4b1d-b646-47c2c3a97792")

    recordDataset(mode, serviceUser, "N18630101S0010.fits").flatMap: (_, did) =>
      assertIO(addWithIdempotencyKey(did, idm = idm).map(_._2), idm)

  test("addDatasetEvent - duplicate idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b7044cd8-38b5-4592-8d99-91d2c512041d")

    recordDataset(mode, serviceUser, "N18630101S0011.fits").flatMap: (_, did) =>
      addWithIdempotencyKey(did, idm = idm).flatMap: (eid, _) =>
        assertIO(addWithIdempotencyKey(did, idm = idm).map(_._1), eid)

  // A client-supplied event time relative to "now", well within the visit's
  // window (the visit was just created).
  private def nowMinus(seconds: Long): Timestamp =
    Timestamp.unsafeFromInstantTruncated(Instant.now().minusSeconds(seconds))

  private def addDatasetEventTimes(
    did:        Dataset.Id,
    stage:      DatasetStage,
    clientTime: Option[Timestamp]
  ): IO[(Timestamp, Option[Timestamp], Timestamp)] =
    query(
      serviceUser,
      s"""
        mutation {
          addDatasetEvent(input: {
            datasetId:    "$did"
            datasetStage: ${stage.tag.toUpperCase}
            ${clientTime.fold("")(t => s"""clientTime: "${t.isoFormat}"""")}
          }) {
            event {
              effectiveTime
              clientTime
              recordedTime
            }
          }
        }
      """
    ).map: json =>
      val c = json.hcursor.downFields("addDatasetEvent", "event")
      (
        c.downField("effectiveTime").require[Timestamp],
        c.downField("clientTime").require[Option[Timestamp]],
        c.downField("recordedTime").require[Timestamp]
      )

  test("addDatasetEvent - supplied time is surfaced and independent of received"):
    val t = nowMinus(60)
    recordDataset(mode, serviceUser, "N18630101S0020.fits").flatMap: (_, did) =>
      addDatasetEventTimes(did, DatasetStage.StartWrite, t.some).map: (time, client, received) =>
        assertEquals(time, t)
        assertEquals(client, t.some)
        assertNotEquals(received, t)

  test("addDatasetEvent - omitted time defaults to received"):
    recordDataset(mode, serviceUser, "N18630101S0021.fits").flatMap: (_, did) =>
      addDatasetEventTimes(did, DatasetStage.StartWrite, None).map: (time, client, received) =>
        assertEquals(client, none)
        assertEquals(time, received)

  test("addDatasetEvent - supplied times drive the dataset interval"):
    val t1 = nowMinus(120)
    val t2 = nowMinus(60)
    for
      ids <- recordDataset(mode, serviceUser, "N18630101S0022.fits")
      (_, did) = ids
      _   <- addDatasetEventTimes(did, DatasetStage.StartExpose, t1.some)
      _   <- addDatasetEventTimes(did, DatasetStage.EndWrite, t2.some)
      ts  <- timestamps(did)
    yield assertEquals(ts, TimestampInterval.between(t1, t2).some)

  private val outOfRange: String =
    "The supplied event time is outside the visit's expected timeframe."

  private def rejectQuery(did: Dataset.Id, t: Timestamp): String =
    s"""
      mutation {
        addDatasetEvent(input: {
          datasetId:    "$did"
          datasetStage: START_WRITE
          clientTime:   "${t.isoFormat}"
        }) {
          event { id }
        }
      }
    """

  test("addDatasetEvent - future time is rejected"):
    val t = Timestamp.unsafeFromInstantTruncated(Instant.now().plusSeconds(3600))
    recordDataset(mode, serviceUser, "N18630101S0023.fits").flatMap: (_, did) =>
      expect(serviceUser, rejectQuery(did, t), List(outOfRange).asLeft)

  test("addDatasetEvent - far-past time is rejected"):
    val t = Timestamp.unsafeFromInstantTruncated(Instant.now().minusSeconds(7200))
    recordDataset(mode, serviceUser, "N18630101S0024.fits").flatMap: (_, did) =>
      expect(serviceUser, rejectQuery(did, t), List(outOfRange).asLeft)