// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.odb.data.StepExecutionState
import lucuma.odb.util.Codecs.*
import munit.Location
import skunk.*
import skunk.implicits.*

// Editing a sequence while one of its steps is executing would modify that
// step, so the edit is rejected until the step ends.  The check is not
// instrument specific, so GMOS North stands in for all of them.
class sequenceEditWhileExecuting
  extends query.ExecutionTestSupportForGmos
     with ReplaceGmosNorthSequenceOps
     with InsertSmartGcalOps
     with UpdateObservationsOps:

  private def rejected(oid: Observation.Id, sequenceType: SequenceType, sid: Step.Id): List[String] =
    List(s"The ${sequenceType.tag} sequence of observation $oid cannot be edited while step $sid is executing.")

  private val setup: IO[(Observation.Id, Visit.Id)] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, o)
    yield (o, v)

  private def firstStep(oid: Observation.Id, sequenceType: SequenceType): IO[Step.Id] =
    val ids = sequenceType match
      case SequenceType.Acquisition => acquisitionSequenceIds(pi, oid)
      case SequenceType.Science     => scienceSequenceIds(pi, oid)
    ids.map(_.values.flatten.head)

  private def startStep(oid: Observation.Id, sequenceType: SequenceType, vid: Visit.Id): IO[Step.Id] =
    firstStep(oid, sequenceType).flatTap(addStepEventAs(serviceUser, _, vid, StepStage.StartStep).void)

  private def replace(oid: Observation.Id, sequenceType: SequenceType, errors: Option[List[String]])(using Location): IO[Unit] =
    val in = input(oid, sequenceType, atomInput("Edited", stepInput(GmosNorthFilter.GPrime)))
    errors.fold(query(staff, mutation(Instrument.GmosNorth, in)).void): es =>
      expect(staff, mutation(Instrument.GmosNorth, in), es.asLeft)

  private def resetAcquisition(oid: Observation.Id, regenerate: Boolean, errors: Option[List[String]])(using Location): IO[Unit] =
    val q = s"""
      mutation {
        resetAcquisition(input: {
          observationId: ${oid.asJson}
          regenerate: ${regenerate.asJson}
        }) {
          observation { id }
        }
      }
    """
    errors.fold(query(serviceUser, q).void)(es => expect(serviceUser, q, es.asLeft))

  private def replaceAbandoning(oid: Observation.Id, sequenceType: SequenceType): IO[Unit] =
    val in = input(oid, sequenceType, atomInput("Edited", stepInput(GmosNorthFilter.GPrime)))
      .replace("sequence:", "abandonOngoingStep: true\n        sequence:")
    query(staff, mutation(Instrument.GmosNorth, in)).void

  private def resetAbandoning(oid: Observation.Id): IO[Unit] =
    query(
      serviceUser,
      s"""
        mutation {
          resetAcquisition(input: {
            observationId: ${oid.asJson}
            abandonOngoingStep: true
          }) {
            observation { id }
          }
        }
      """
    ).void

  private val SelectStepExecutionState: Query[Step.Id, StepExecutionState] =
    sql"SELECT c_execution_state FROM t_step_execution WHERE c_step_id = $step_id".query(step_execution_state)

  private def expectAbandoned(sid: Step.Id)(using Location): IO[Unit] =
    withSession(_.unique(SelectStepExecutionState)(sid)).map(assertEquals(_, StepExecutionState.Abandoned))

  private def editAcquisition(oid: Observation.Id, errors: Option[List[String]])(using Location): IO[Unit] =
    val q = updateObservationsMutation(
      oid,
      """
        observingMode: {
          gmosNorthLongSlit: {
            acquisition: {
              explicitFilter: I_PRIME
            }
          }
        }
      """,
      "observations { id }"
    )
    errors.fold(query(staff, q).void)(es => expect(staff, q, es.asLeft))

  test("replacing the science sequence is rejected while a science step is executing"):
    for
      (o, v) <- setup
      s      <- startStep(o, SequenceType.Science, v)
      _      <- replace(o, SequenceType.Science, rejected(o, SequenceType.Science, s).some)
      _      <- addEndStepEvent(s, v)
      _      <- replace(o, SequenceType.Science, none)
    yield ()

  test("replacing the acquisition is rejected while an acquisition step is executing"):
    for
      (o, v) <- setup
      s      <- startStep(o, SequenceType.Acquisition, v)
      _      <- replace(o, SequenceType.Acquisition, rejected(o, SequenceType.Acquisition, s).some)
      _      <- addEndStepEvent(s, v)
      _      <- replace(o, SequenceType.Acquisition, none)
    yield ()

  test("the science sequence may be replaced while an acquisition step is executing"):
    for
      (o, v) <- setup
      _      <- startStep(o, SequenceType.Acquisition, v)
      _      <- replace(o, SequenceType.Science, none)
    yield ()

  test("resetting the acquisition is rejected while an acquisition step is executing"):
    for
      (o, v) <- setup
      s      <- startStep(o, SequenceType.Acquisition, v)
      _      <- resetAcquisition(o, regenerate = false, rejected(o, SequenceType.Acquisition, s).some)
      _      <- resetAcquisition(o, regenerate = true,  rejected(o, SequenceType.Acquisition, s).some)
      _      <- addEndStepEvent(s, v)
      _      <- resetAcquisition(o, regenerate = false, none)
    yield ()

  test("resetting a customized acquisition is rejected while an acquisition step is executing"):
    for
      (o, v) <- setup
      _      <- replace(o, SequenceType.Acquisition, none)
      s      <- startStep(o, SequenceType.Acquisition, v)
      _      <- resetAcquisition(o, regenerate = false, rejected(o, SequenceType.Acquisition, s).some)
      _      <- addEndStepEvent(s, v)
      _      <- resetAcquisition(o, regenerate = false, none)
    yield ()

  test("editing the acquisition parameters is rejected while an acquisition step is executing"):
    for
      (o, v) <- setup
      s      <- startStep(o, SequenceType.Acquisition, v)
      _      <- editAcquisition(o, rejected(o, SequenceType.Acquisition, s).some)
      _      <- addEndStepEvent(s, v)
      _      <- editAcquisition(o, none)
    yield ()

  test("editing the acquisition parameters is allowed while a science step is executing"):
    for
      (o, v) <- setup
      _      <- startStep(o, SequenceType.Science, v)
      _      <- editAcquisition(o, none)
    yield ()

  // Inserting steps after the executing step leaves that step alone.
  test("smart GCAL steps may be inserted while a step is executing"):
    for
      (o, v) <- setup
      s      <- startStep(o, SequenceType.Science, v)
      _      <- insertAs(staff, o, List(SmartGcalType.Arc), after = s.some)
    yield ()

  test("abandonOngoingStep replaces the sequence anyway, abandoning the executing step"):
    for
      (o, v) <- setup
      s      <- startStep(o, SequenceType.Science, v)
      _      <- replaceAbandoning(o, SequenceType.Science)
      _      <- expectAbandoned(s)
      ids    <- scienceSequenceIds(pi, o)
    yield assert(!ids.values.flatten.toList.contains(s))

  test("abandonOngoingStep resets the acquisition anyway, abandoning the executing step"):
    for
      (o, v) <- setup
      s      <- startStep(o, SequenceType.Acquisition, v)
      _      <- resetAbandoning(o)
      _      <- expectAbandoned(s)
    yield ()

  test("abandonOngoingStep resets a customized acquisition anyway, abandoning the executing step"):
    for
      (o, v) <- setup
      _      <- replace(o, SequenceType.Acquisition, none)
      s      <- startStep(o, SequenceType.Acquisition, v)
      _      <- resetAbandoning(o)
      _      <- expectAbandoned(s)
    yield ()
