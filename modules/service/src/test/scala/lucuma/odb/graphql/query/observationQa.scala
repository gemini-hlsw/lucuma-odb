// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservationQaState
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError

class observationQa extends OdbSuite with ExecutionQuerySetupOperations
                                     with ExecutionTestSupportForGmos {

  val mode: ObservingModeType = ObservingModeType.GmosNorthLongSlit

  case class Counts(total: Int, pass: Int, usable: Int, fail: Int, notAssessed: Int)

  object Counts:
    val Zero: Counts = Counts(0, 0, 0, 0, 0)

  def qaJson(
    state:    Option[ObservationQaState],
    computed: Option[ObservationQaState],
    over:     Option[ObservationQaState],
    counts:   Counts
  ): Json =
    def enc(s: Option[ObservationQaState]): Json =
      s.fold(Json.Null)(_.tag.toScreamingSnakeCase.asJson)

    json"""
      {
        "observation": {
          "qa": {
            "state": ${enc(state)},
            "computed": ${enc(computed)},
            "override": ${enc(over)},
            "datasets": {
              "total": ${counts.total},
              "pass": ${counts.pass},
              "usable": ${counts.usable},
              "fail": ${counts.fail},
              "notAssessed": ${counts.notAssessed}
            }
          }
        }
      }
    """

  def qaQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          qa {
            state
            computed
            override
            datasets { total pass usable fail notAssessed }
          }
        }
      }
    """

  def expectQa(
    user:     User,
    oid:      Observation.Id,
    state:    Option[ObservationQaState],
    computed: Option[ObservationQaState],
    over:     Option[ObservationQaState],
    counts:   Counts
  ): IO[Unit] =
    expect(user, qaQuery(oid), qaJson(state, computed, over, counts).asRight)

  // Computed and effective state agree when there is no override.
  def expectComputed(oid: Observation.Id, state: Option[ObservationQaState], counts: Counts): IO[Unit] =
    expectQa(pi, oid, state, state, None, counts)

  def setOverrideMutation(oid: Observation.Id, qa: Option[ObservationQaState]): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: { qaStateOverride: ${qa.fold("null")(_.tag.toScreamingSnakeCase)} }
          WHERE: { id: { EQ: "$oid" } }
        }) {
          observations { id }
        }
      }
    """

  def setOverride(user: User, oid: Observation.Id, qa: Option[ObservationQaState]): IO[Unit] =
    expect(user, setOverrideMutation(oid, qa), json"""{ "updateObservations": { "observations": [ { "id": $oid } ] } }""".asRight)

  // Records an observation with `n` science datasets, all initially not assessed.
  def setup(offset: Int, n: Int): IO[(Observation.Id, List[Dataset.Id])] =
    recordAll(pi, serviceUser, mode, offset = offset, datasetCount = n).map: on =>
      (on.id, on.allDatasets)

  def setQa(qa: DatasetQaState, dids: List[Dataset.Id]): IO[Unit] =
    updateDatasets(staff, qa, dids)

  import ObservationQaState.*

  test("no datasets: no verdict"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationAs(pi, pid, mode.some, tid)
      _   <- expectComputed(oid, None, Counts.Zero)
    yield ()

  test("datasets not yet assessed: no verdict"):
    setup(0, 3).flatMap: (oid, _) =>
      expectComputed(oid, None, Counts(3, 0, 0, 0, 3))

  test("any dataset not yet assessed: no verdict"):
    setup(10, 3).flatMap: (oid, ds) =>
      setQa(DatasetQaState.Pass, ds.take(2)) *>
        expectComputed(oid, None, Counts(3, 2, 0, 0, 1))

  test("all pass: PASS"):
    setup(20, 3).flatMap: (oid, ds) =>
      setQa(DatasetQaState.Pass, ds) *>
        expectComputed(oid, Pass.some, Counts(3, 3, 0, 0, 0))

  test("all fail: FAIL"):
    setup(30, 3).flatMap: (oid, ds) =>
      setQa(DatasetQaState.Fail, ds) *>
        expectComputed(oid, Fail.some, Counts(3, 0, 0, 3, 0))

  test("all usable: USABLE"):
    setup(40, 2).flatMap: (oid, ds) =>
      setQa(DatasetQaState.Usable, ds) *>
        expectComputed(oid, Usable.some, Counts(2, 0, 2, 0, 0))

  test("pass and fail mixed: USABLE"):
    setup(50, 3).flatMap: (oid, ds) =>
      setQa(DatasetQaState.Pass, ds.take(2)) *>
        setQa(DatasetQaState.Fail, ds.drop(2)) *>
        expectComputed(oid, Usable.some, Counts(3, 2, 0, 1, 0))

  // Starts step `sid` in a new visit.
  def startStep(oid: Observation.Id, sid: Step.Id): IO[Visit.Id] =
    for
      vid <- recordVisitAs(serviceUser, oid)
      _   <- addStepEventAs(serviceUser, sid, vid, StepStage.StartStep)
    yield vid

  // Records a dataset for step `sid`, through to the given dataset stages.
  def recordExtraDataset(
    vid:    Visit.Id,
    sid:    Step.Id,
    index:  Int,
    stages: List[DatasetStage]
  ): IO[Dataset.Id] =
    for
      did <- recordDatasetAs(serviceUser, sid, vid, f"$DatasetFilenamePrefix$index%04d.fits")
      _   <- stages.traverse_(addDatasetEventAs(serviceUser, did, _))
    yield did

  val Written: List[DatasetStage] =
    List(DatasetStage.StartExpose, DatasetStage.EndExpose, DatasetStage.StartReadout, DatasetStage.EndReadout, DatasetStage.StartWrite, DatasetStage.EndWrite)

  test("acquisition datasets are ignored"):
    for
      (oid, ds) <- setup(60, 2)
      _         <- setQa(DatasetQaState.Pass, ds)
      acq       <- acquisitionSequenceIds(serviceUser, oid)
      sid        = acq.head._2.head
      vid       <- startStep(oid, sid)
      did       <- recordExtraDataset(vid, sid, 69, Written)
      _         <- setQa(DatasetQaState.Fail, List(did))
      _         <- expectComputed(oid, Pass.some, Counts(2, 2, 0, 0, 0))
    yield ()

  test("datasets that were never written are ignored"):
    for
      (oid, ds) <- setup(140, 2)
      _         <- setQa(DatasetQaState.Pass, ds)
      sci       <- scienceSequenceIds(serviceUser, oid)
      sid        = sci.head._2.head
      vid       <- startStep(oid, sid)
      _         <- recordExtraDataset(vid, sid, 149, List(DatasetStage.StartExpose))  // e.g. aborted
      _         <- expectComputed(oid, Pass.some, Counts(2, 2, 0, 0, 0))
      _         <- recordExtraDataset(vid, sid, 148, Written)                         // written, not assessed
      _         <- expectComputed(oid, None, Counts(3, 2, 0, 0, 1))
    yield ()

  test("staff override replaces the calculated state, and clearing it restores it"):
    for
      (oid, ds) <- setup(70, 2)
      _         <- setQa(DatasetQaState.Fail, ds)
      _         <- setOverride(staff, oid, Pass.some)
      _         <- expectQa(pi, oid, Pass.some, Fail.some, Pass.some, Counts(2, 0, 0, 2, 0))
      _         <- setOverride(staff, oid, None)
      _         <- expectComputed(oid, Fail.some, Counts(2, 0, 0, 2, 0))
    yield ()

  test("override stays in place as dataset QA changes"):
    for
      (oid, ds) <- setup(80, 2)
      _         <- setOverride(staff, oid, Usable.some)
      _         <- expectQa(pi, oid, Usable.some, None, Usable.some, Counts(2, 0, 0, 0, 2))
      _         <- setQa(DatasetQaState.Pass, ds)
      _         <- expectQa(pi, oid, Usable.some, Pass.some, Usable.some, Counts(2, 2, 0, 0, 0))
    yield ()

  test("override may be set on an observation without datasets"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationAs(pi, pid, mode.some, tid)
      _   <- setOverride(staff, oid, Fail.some)
      _   <- expectQa(pi, oid, Fail.some, None, Fail.some, Counts.Zero)
    yield ()

  test("PI cannot set the override"):
    setup(90, 1).flatMap: (oid, _) =>
      expectOdbError(pi, setOverrideMutation(oid, Pass.some), { case OdbError.NotAuthorized(_, _) => () }) *>
        expectOdbError(pi, setOverrideMutation(oid, None), { case OdbError.NotAuthorized(_, _) => () }) *>
        expectComputed(oid, None, Counts(1, 0, 0, 0, 1))

  def createWithOverrideMutation(pid: Program.Id, qa: ObservationQaState): String =
    s"""
      mutation {
        createObservation(input: {
          programId: "$pid"
          SET: { qaStateOverride: ${qa.tag.toScreamingSnakeCase} }
        }) {
          observation { id }
        }
      }
    """

  test("staff may set the override when creating an observation"):
    for
      pid <- createProgramAs(pi)
      oid <- query(staff, createWithOverrideMutation(pid, Usable)).map: js =>
               js.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
      _   <- expectQa(pi, oid, Usable.some, None, Usable.some, Counts.Zero)
    yield ()

  test("PI cannot set the override when creating an observation"):
    createProgramAs(pi).flatMap: pid =>
      expectOdbError(pi, createWithOverrideMutation(pid, Pass), { case OdbError.NotAuthorized(_, _) => () })

  test("staff may override a calibration observation's QA state"):
    for
      (oid, _) <- setup(100, 1)
      _        <- setObservationCalibrationRole(List(oid), CalibrationRole.SpectroPhotometric)
      _        <- setOverride(staff, oid, Fail.some)
      _        <- expectQa(pi, oid, Fail.some, None, Fail.some, Counts(1, 0, 0, 0, 1))
    yield ()

  test("filter observations by QA"):
    for
      (oA, dA) <- setup(110, 2)
      _        <- setQa(DatasetQaState.Pass, dA.take(1))
      _        <- setQa(DatasetQaState.Fail, dA.drop(1))      // USABLE, no override
      (oB, dB) <- setup(120, 2)
      _        <- setQa(DatasetQaState.Pass, dB.take(1))
      _        <- setQa(DatasetQaState.Fail, dB.drop(1))
      _        <- setOverride(staff, oB, Pass.some)           // USABLE, overridden PASS
      (oC, dC) <- setup(130, 2)
      _        <- setQa(DatasetQaState.Pass, dC.take(1))      // no verdict, 1 not assessed
      pid      <- createProgramAs(pi)
      tid      <- createTargetWithProfileAs(pi, pid)
      oD       <- createObservationAs(pi, pid, mode.some, tid)
      _        <- setOverride(staff, oD, Fail.some)           // no datasets, overridden FAIL
      all       = List(oA, oB, oC, oD)
      ids       = (qa: String) =>
        query(staff, s"""
          query {
            observations(WHERE: { id: { IN: ${all.asJson} }, qa: $qa }) {
              matches { id }
            }
          }
        """).map: js =>
          js.hcursor.downFields("observations", "matches").values.toList.flatten.map(_.hcursor.downField("id").require[Observation.Id]).toSet
      _        <- assertIO(ids("{ state: { EQ: FAIL } }"), Set(oD))
      _        <- assertIO(ids("{ state: { IN: [PASS, FAIL] } }"), Set(oB, oD))
      _        <- assertIO(ids("{ state: { IS_NULL: true } }"), Set(oC))
      _        <- assertIO(ids("{ computed: { EQ: USABLE } }"), Set(oA, oB))
      _        <- assertIO(ids("{ computed: { EQ: USABLE }, override: { IS_NULL: true } }"), Set(oA))
      _        <- assertIO(ids("{ override: { IS_NULL: false } }"), Set(oB, oD))
      _        <- assertIO(ids("{ datasets: { notAssessed: { GT: 0 } } }"), Set(oC))
      _        <- assertIO(ids("{ datasets: { total: { EQ: 0 } } }"), Set(oD))
      _        <- assertIO(ids("{ datasets: { fail: { GTE: 1 } }, state: { NEQ: PASS } }"), Set(oA))
    yield ()

}
