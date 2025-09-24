// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.Client
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval


class addDatasetEvent extends OdbSuite {

  val mode: ObservingModeType = ObservingModeType.GmosNorthLongSlit
  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordDataset(
    mode: ObservingModeType,
    user: User,
    file: String
  ):IO[(Observation.Id, Dataset.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
      did <- recordDatasetAs(user, sid, file)
    } yield (oid, did)

  private def addDatasetEventTest(
    mode:     ObservingModeType,
    user:     User,
    file:     String,
    query:    Dataset.Id => String,
    expected: (Observation.Id, Dataset.Id) => Either[String, Json]
  ): IO[Unit] =
    for {
      ids <- recordDataset(mode, user, file)
      (oid, did) = ids
      _   <- expect(user, query(did), expected(oid, did).leftMap(s => List(s)))
    } yield ()

  test("addDatasetEvent") {
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
      service,
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

  }

  test("addDatasetEvent - with filename") {
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
      service,
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

  }

  test("addDatasetEvent - unknown dataset") {
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
      service,
      "N18630101S0003.fits",
      _ => query,
      (_, _) => s"Dataset 'd-1863' not found".asLeft
    )

  }

  private def addEvent(did: Dataset.Id, stage: DatasetStage): IO[Timestamp] =
    query(
      service,
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
    ).map { json =>
      json.hcursor.downFields("addDatasetEvent", "event", "received").require[Timestamp]
    }

  private def timestamps(did: Dataset.Id): IO[Option[TimestampInterval]] =
    query(
      service,
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
    ).map { json =>
      val d = json.hcursor.downFields("dataset", "interval").success.filter(!_.value.isNull)
      val s = d.flatMap(_.downField("start").require[Option[Timestamp]])
      val e = d.flatMap(_.downField("end").require[Option[Timestamp]])
      (s, e).mapN { (start, end) => TimestampInterval.between(start, end) }
    }

  private def timeTest(file: String, stages: DatasetStage*): IO[Unit] = {
    def expected(times: List[Timestamp]): (Option[Timestamp], Option[Timestamp]) =
      times.zip(stages).foldLeft((Option.empty[Timestamp], Option.empty[Timestamp])) { case ((start, end), (time, stage)) =>
        if (stage === DatasetStage.StartExpose) (time.some, none)
        else if ((stage === DatasetStage.EndWrite) && start.isDefined) (start, time.some)
        else (start, end)
      }

    for {
      ids <- recordDataset(mode, service, file)
      (oid, did) = ids
      es  <- stages.toList.traverse(addEvent(did, _))
      ex   = expected(es).mapN { (s, e) => TimestampInterval.between(s, e) }
      ts  <- timestamps(did)
    } yield assertEquals(ts, ex)
  }

  test("addDatasetEvent - no start time") {
    timeTest("N18630101S0004.fits", DatasetStage.StartWrite)
  }

  test("addDatasetEvent - start") {
    timeTest("N18630101S0005.fits", DatasetStage.StartExpose)
  }

  test("addDatasetEvent - start, end") {
    timeTest("N18630101S0006.fits", DatasetStage.StartExpose, DatasetStage.EndWrite)
  }

  test("addDatasetEvent - end, no start") {
    timeTest("N18630101S0007.fits", DatasetStage.EndWrite)
  }

  test("addDatasetEvent - start, end, start") {
    timeTest("N18630101S0008.fits", DatasetStage.StartExpose, DatasetStage.EndWrite, DatasetStage.StartExpose)
  }

  def addWithIdempotencyKey(
    did: Dataset.Id,
    idm: Option[IdempotencyKey] = None,
    cid: Option[Client.Id]      = None
  ): IO[(ExecutionEvent.Id, Option[IdempotencyKey], Option[Client.Id])] =
      query(
        service,
        s"""
          mutation {
            addDatasetEvent(input: {
              datasetId: "$did"
              datasetStage: START_WRITE
              ${idm.fold("")(idm => s"idempotencyKey: \"$idm\"")}
              ${cid.fold("")(cid => s"clientId: \"$cid\"")}
            }) {
              event {
                id
                idempotencyKey
                clientId
              }
            }
          }
        """
      ).flatMap: js =>
        val cur = js.hcursor.downFields("addDatasetEvent", "event")
        (for
          e <- cur.downField("id").as[ExecutionEvent.Id]
          n <- cur.downField("idempotencyKey").as[Option[IdempotencyKey]]
          d <- cur.downField("clientId").as[Option[Client.Id]]
        yield (e, n, d)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  test("addDatasetEvent - client id"):
    val cid  = Client.Id.parse("c-530c979f-de98-472f-9c23-a3442f2a9f7f")

    recordDataset(mode, service, "N18630101S0009.fits").flatMap: (_, did) =>
      assertIO(addWithIdempotencyKey(did, cid = cid).map(_._3), cid)

  test("addDatasetEvent - idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b9bac66c-4e12-4b1d-b646-47c2c3a97792")

    recordDataset(mode, service, "N18630101S0010.fits").flatMap: (_, did) =>
      assertIO(addWithIdempotencyKey(did, idm = idm).map(_._2), idm)

  test("addDatasetEvent - duplicate idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b7044cd8-38b5-4592-8d99-91d2c512041d")

    recordDataset(mode, service, "N18630101S0011.fits").flatMap: (_, did) =>
      addWithIdempotencyKey(did, idm = idm).flatMap: (eid, _, _) =>
        assertIO(addWithIdempotencyKey(did, idm = idm).map(_._1), eid)

}
