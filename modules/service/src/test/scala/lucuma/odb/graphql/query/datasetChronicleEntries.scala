// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.order.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.sequence.Dataset
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.itc.IntegrationTime
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.dataset_id
import skunk.codec.temporal.timestamptz
import skunk.syntax.all.*


class datasetChronicleEntries extends OdbSuite with DatasetSetupOperations with ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  val mode = ObservingModeType.GmosNorthLongSlit

  def setInterval(did: Dataset.Id, interval: TimestampInterval): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""
          UPDATE t_dataset
             SET c_start_time = $core_timestamp,
                 c_end_time   = $core_timestamp
           WHERE c_dataset_id = $dataset_id
        """.command
      )(interval.start, interval.end, did)
    .void

  test("Basic DatasetChronicleEntry Mapping"):
    for
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithUsPi(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      _   <- addQueueProposal(pi, pid, cfp)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- runObscalcUpdate(pid, oid)
      _   <- submitProposal(pi, pid)
      _   <- acceptProposal(staff, pid)
      ref <- fetchProgramReference(pi, pid)

      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      aid <- recordAtomAs(serviceUser, mode.instrument, vid)
      sid <- recordStepAs(serviceUser, mode.instrument, aid)
      did <- recordDatasetAs(serviceUser, sid, "N18630703S0001.fits")
      t0   = Timestamp.FromString.getOption("2025-07-30T23:00:00Z").get
      t1   = Timestamp.FromString.getOption("2025-07-30T23:00:10Z").get
      _   <- setInterval(did, TimestampInterval.between(t0, t1))
      _   <- updateDatasets(staff, DatasetQaState.Pass, List(did))

      _   <- expect(
        staff,
        s"""
          query {
            datasetChronicleEntries() {
              hasMore
              matches {
                operation
                dataset { id }
                modDatasetId
                modStepId
                modObservationId
                modVisitId
                modReference
                modFilename
                modQaState
                modInterval
                modComment
                newDatasetId
                newStepId
                newObservationId
                newVisitId
                newReference
                newFilename
                newQaState
                newInterval { duration { seconds } }
                newComment
              }
            }
          }
        """,
        json"""
          {
            "datasetChronicleEntries": {
              "hasMore": false,
              "matches": [
                {
                  "operation":        "INSERT",
                  "dataset":          { "id": $did },
                  "modDatasetId":     true,
                  "modStepId":        true,
                  "modObservationId": true,
                  "modVisitId":       true,
                  "modReference":     true,
                  "modFilename":      true,
                  "modQaState":       false,
                  "modInterval":      false,
                  "modComment":       false,
                  "newDatasetId":     $did,
                  "newStepId":        $sid,
                  "newObservationId": $oid,
                  "newVisitId":       $vid,
                  "newReference":     "G-2025A-0001-Q-0001-0001-0001",
                  "newFilename":      "N18630703S0001.fits",
                  "newQaState":       null,
                  "newInterval":      null,
                  "newComment":       null
                },
                {
                  "operation":        "UPDATE",
                  "dataset":          { "id": $did },
                  "modDatasetId":     false,
                  "modStepId":        false,
                  "modObservationId": false,
                  "modVisitId":       false,
                  "modReference":     false,
                  "modFilename":      false,
                  "modQaState":       false,
                  "modInterval":      true,
                  "modComment":       false,
                  "newDatasetId":     null,
                  "newStepId":        null,
                  "newObservationId": null,
                  "newVisitId":       null,
                  "newReference":     null,
                  "newFilename":      null,
                  "newQaState":       null,
                  "newInterval":      { "duration": { "seconds": 10.000000 }},
                  "newComment":       null
                },
                {
                  "operation":        "UPDATE",
                  "dataset":          { "id": $did },
                  "modDatasetId":     false,
                  "modStepId":        false,
                  "modObservationId": false,
                  "modVisitId":       false,
                  "modReference":     false,
                  "modFilename":      false,
                  "modQaState":       true,
                  "modInterval":      false,
                  "modComment":       false,
                  "newDatasetId":     null,
                  "newStepId":        null,
                  "newObservationId": null,
                  "newVisitId":       null,
                  "newReference":     null,
                  "newFilename":      null,
                  "newQaState":       "PASS",
                  "newInterval":      null,
                  "newComment":       null
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("No Reference"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      aid <- recordAtomAs(serviceUser, mode.instrument, vid)
      sid <- recordStepAs(serviceUser, mode.instrument, aid)
      did <- recordDatasetAs(serviceUser, sid, "N18630703S0002.fits")
      _   <- expect(
        staff,
        s"""
          query {
            datasetChronicleEntries(WHERE: { dataset: { EQ: "$did" } } ) {
              hasMore
              matches {
                operation
                dataset { id }
                modDatasetId
                modStepId
                modObservationId
                modVisitId
                modReference
                modFilename
                modQaState
                modInterval
                modComment
                newDatasetId
                newStepId
                newObservationId
                newVisitId
                newReference
                newFilename
                newQaState
                newInterval { duration { seconds } }
                newComment
              }
            }
          }
        """,
        json"""
          {
            "datasetChronicleEntries": {
              "hasMore": false,
              "matches": [
                {
                  "operation":        "INSERT",
                  "dataset":          { "id": $did },
                  "modDatasetId":     true,
                  "modStepId":        true,
                  "modObservationId": true,
                  "modVisitId":       true,
                  "modReference":     false,
                  "modFilename":      true,
                  "modQaState":       false,
                  "modInterval":      false,
                  "modComment":       false,
                  "newDatasetId":     $did,
                  "newStepId":        $sid,
                  "newObservationId": $oid,
                  "newVisitId":       $vid,
                  "newReference":     null,
                  "newFilename":      "N18630703S0002.fits",
                  "newQaState":       null,
                  "newInterval":      null,
                  "newComment":       null
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  def comment(
    comment: String,
    dids:    List[Dataset.Id]
  ): IO[Unit] =
    val q = s"""
      mutation {
        updateDatasets(input: {
          SET: {
            comment: "${comment}"
          },
          WHERE: {
            id: { IN: [ ${dids.map(_.show).mkString("\"", "\",\"", "\"")} ] }
          }
        }) {
          datasets {
            id
          }
        }
      }
    """
    query(user = staff, query = q).void

  def setup(index: Int): IO[Dataset.Id] =
    for
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithUsPi(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      _   <- addQueueProposal(pi, pid, cfp)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- runObscalcUpdate(pid, oid)
      _   <- submitProposal(pi, pid)
      _   <- acceptProposal(staff, pid)
      ref <- fetchProgramReference(pi, pid)

      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      aid <- recordAtomAs(serviceUser, mode.instrument, vid)
      sid <- recordStepAs(serviceUser, mode.instrument, aid)
      did <- recordDatasetAs(serviceUser, sid, f"N18630703S$index%04d.fits")
      t0   = Timestamp.FromString.getOption("2025-07-30T23:00:00Z").get
      t1   = Timestamp.FromString.getOption("2025-07-30T23:00:10Z").get
      _   <- setInterval(did, TimestampInterval.between(t0, t1))
      _   <- updateDatasets(staff, DatasetQaState.Pass, List(did))
      _   <- comment("foo", List(did))
    yield did

  test("WHERE user"):
    setup(3).flatMap: did =>
      expect(
        staff,
        s"""
          query {
            datasetChronicleEntries(WHERE: {
              user: { id: { EQ: "${staff.id}" } }
              dataset: { EQ: "$did" }
            }) {
              hasMore
              matches {
                dataset { id }
                modQaState
                modComment
              }
            }
          }
        """,
        json"""
          {
            "datasetChronicleEntries": {
              "hasMore": false,
              "matches": [
                {
                  "dataset":   { "id": $did },
                  "modQaState": true,
                  "modComment": false
                },
                {
                  "dataset":   { "id": $did },
                  "modQaState": false,
                  "modComment": true
                }
              ]
            }
          }
        """.asRight
      )

  test("WHERE operation"):
    setup(4).flatMap: did =>
      expect(
        staff,
        s"""
          query {
            datasetChronicleEntries(WHERE: {
              operation: { EQ: INSERT }
              dataset: { EQ: "$did" }
            }) {
              hasMore
              matches {
                operation
                dataset { id }
              }
            }
          }
        """,
        json"""
          {
            "datasetChronicleEntries": {
              "hasMore": false,
              "matches": [
                {
                  "operation": "INSERT",
                  "dataset":   { "id": $did }
                }
              ]
            }
          }
        """.asRight
      )

  val currentTimestamp: IO[Timestamp] =
    withSession: s =>
      s.unique:
        sql"""SELECT CURRENT_TIMESTAMP(6)"""
          .query(timestamptz(6))
          .map(t => Timestamp.unsafeFromInstantTruncated(t.toInstant))

  test("WHERE timestamp"):
    currentTimestamp.flatMap: t =>
      setup(5).flatMap: _ => // we will filter only on timestamp (not dataset id), previous entries ignored
        expect(
          staff,
          s"""
            query {
              datasetChronicleEntries(WHERE: {
                timestamp: { GT: "${Timestamp.FromString.reverseGet(t)}" }
              }) {
                hasMore
                matches {
                  operation
                }
              }
            }
          """,
          json"""
            {
              "datasetChronicleEntries": {
                "hasMore": false,
                "matches": [
                  {
                    "operation": "INSERT"
                  },
                  {
                    "operation": "UPDATE"
                  },
                  {
                    "operation": "UPDATE"
                  },
                  {
                    "operation": "UPDATE"
                  }
                ]
              }
            }
          """.asRight
        )

  test("WHERE modQa"):
    setup(6).flatMap: did =>
      expect(
        staff,
        s"""
          query {
            datasetChronicleEntries(WHERE: {
              dataset: { EQ: "$did" }
              modQaState: { EQ: true }
            }) {
              hasMore
              matches {
                operation
                modQaState
                newQaState
              }
            }
          }
        """,
        json"""
          {
            "datasetChronicleEntries": {
              "hasMore": false,
              "matches": [
                {
                  "operation": "UPDATE",
                  "modQaState": true,
                  "newQaState": "PASS"
                }
              ]
            }
          }
        """.asRight
      )

  test("WHERE modComment"):
    setup(7).flatMap: did =>
      expect(
        staff,
        s"""
          query {
            datasetChronicleEntries(WHERE: {
              dataset: { EQ: "$did" }
              modComment: { EQ: true }
            }) {
              hasMore
              matches {
                operation
                modComment
                newComment
              }
            }
          }
        """,
        json"""
          {
            "datasetChronicleEntries": {
              "hasMore": false,
              "matches": [
                {
                  "operation": "UPDATE",
                  "modComment": true,
                  "newComment": "foo"
                }
              ]
            }
          }
        """.asRight
      )

  test("can read timestamp"):
    currentTimestamp.flatMap: t =>
      setup(8).flatMap: did =>
        val timestamps = query(
          staff,
          s"""
            query {
              datasetChronicleEntries(WHERE: {
                dataset: { EQ: "$did" }
                modQaState: { EQ: true }
              }) {
                matches { timestamp }
              }
            }
          """
        ).flatMap: js =>
          js.hcursor
            .downFields("datasetChronicleEntries", "matches")
            .values
            .toList
            .flatten
            .traverse: js =>
               js.hcursor.downField("timestamp").as[Timestamp]
            .leftMap(f => new RuntimeException(f.message))
            .liftTo[IO]
            .map(_.map(_ > t)) // don't know the precise timestamp, but it should be readable and > than t

        assertIO(timestamps, List(true))