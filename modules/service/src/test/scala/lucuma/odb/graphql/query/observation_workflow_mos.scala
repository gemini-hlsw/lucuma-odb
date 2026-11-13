// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.service.workflow.validator.PosAngleValidator

/**
 * MOS observations checked against `PosAngleValidator`.
 */
class observation_workflow_mos extends ExecutionTestSupportForGmos with MosMaskSupport:

  private def setPosAngle(oid: Observation.Id, mode: String, degrees: Int): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              posAngleConstraint: { mode: $mode, angle: { degrees: $degrees } }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def validations(oid: Observation.Id): IO[List[(ObservationValidationCode, List[String])]] =
    queryObservationValidationsAs(pi, oid).map:
      _.filter: (code, _) =>
        code === ObservationValidationCode.ConfigurationError || code === ObservationValidationCode.ConfigurationWarning

  private def error(msgs: String*): (ObservationValidationCode, List[String]) =
    (ObservationValidationCode.ConfigurationError, msgs.toList)

  private def warning(msgs: String*): (ObservationValidationCode, List[String]) =
    (ObservationValidationCode.ConfigurationWarning, msgs.toList)

  private def setup: IO[(Program.Id, Target.Id)] =
    for
      pid <- createProgram
      tid <- createTargetWithProfileAs(pi, pid)
    yield (pid, tid)

  private def expectValidations(
    pid:  Program.Id,
    oid:  Observation.Id,
    expected: (ObservationValidationCode, List[String])*
  ): IO[Unit] =
    for
      _  <- runObscalcUpdate(pid, oid)
      vs <- validations(oid)
    yield assertEquals(vs, expected.toList)

  test("a GMOS MOS observation with an unfixed position angle is an error"):
    for
      (pid, tid) <- setup
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(none))
      _          <- setPosAngle(oid, "AVERAGE_PARALLACTIC", 0)
      _          <- expectValidations(pid, oid, error(PosAngleValidator.ModeMessage))
    yield ()

  test("a Flamingos-2 MOS observation that allows flipping is an error"):
    for
      (pid, tid) <- setup
      oid        <- createObservationWithModeAs(pi, pid, List(tid), flamingos2MosMode(none))
      _          <- setPosAngle(oid, "ALLOW_FLIP", 0)
      _          <- expectValidations(pid, oid, error(PosAngleValidator.ModeMessage))
    yield ()

  test("a fixed angle that differs from the mask's angle is a warning"):
    for
      (pid, tid) <- setup
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth, Angle.fromDoubleDegrees(45.0).some)
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(aid.some))
      _          <- setPosAngle(oid, "FIXED", 30)
      _          <- expectValidations(pid, oid, warning(PosAngleValidator.angleMessage(Angle.fromDoubleDegrees(45.0), Angle.fromDoubleDegrees(30.0))))
    yield ()

  test("a fixed angle that matches the mask's angle is valid"):
    for
      (pid, tid) <- setup
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-02_ODF.fits", Instrument.GmosNorth, Angle.fromDoubleDegrees(45.0).some)
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(aid.some))
      _          <- setPosAngle(oid, "FIXED", 45)
      _          <- expectValidations(pid, oid)
    yield ()

  test("a mask without a parsed design produces no angle warning"):
    for
      (pid, tid) <- setup
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-03_ODF.fits", Instrument.GmosNorth)
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(aid.some))
      _          <- setPosAngle(oid, "FIXED", 30)
      _          <- expectValidations(pid, oid)
    yield ()

  test("a long slit observation may use any position angle mode"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setPosAngle(oid, "AVERAGE_PARALLACTIC", 0)
      _          <- expectValidations(pid, oid)
    yield ()

  private def stateAndTransitions(pid: Program.Id, oid: Observation.Id): IO[(ObservationWorkflowState, List[ObservationWorkflowState])] =
    runObscalcUpdate(pid, oid) *> queryObservationWorkflowStateAndTransitionsAs(pi, oid)

  private def acceptedMosObservation(maskAngle: Option[Angle]): IO[(Program.Id, Observation.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      aid <- maskAngle.traverse(a => insertMosMaskAttachment(pid, "GN2025AQ001-05_ODF.fits", Instrument.GmosNorth, a.some))
      _   <- addProposal(pi, pid, cfp.some, None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(aid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- computeItcResultAs(pi, oid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band1, 4.hourTimeSpan)))
      _   <- setScienceBandAs(pi, oid, ScienceBand.Band1.some)
    yield (pid, oid)

  import ObservationWorkflowState.*

  test("transitions: the mask angle warning keeps the observation Defined but withholds Ready"):
    for
      (pid, oid) <- acceptedMosObservation(Angle.fromDoubleDegrees(45.0).some)
      _          <- setPosAngle(oid, "FIXED", 30)
      withWarn   <- stateAndTransitions(pid, oid)
      _          <- setPosAngle(oid, "FIXED", 45)
      matched    <- stateAndTransitions(pid, oid)
    yield
      assertEquals(withWarn, (Defined, List(Inactive)))
      assertEquals(matched, (Defined, List(Inactive, Ready)))

  test("transitions: unfixing a Ready observation suppresses Ready until it is fixed again"):
    for
      (pid, oid) <- acceptedMosObservation(none)
      _          <- stateAndTransitions(pid, oid)
      _          <- setObservationWorkflowState(pi, oid, Ready)
      unfixed    <- setPosAngle(oid, "UNBOUNDED", 0) *> stateAndTransitions(pid, oid)
      refixed    <- setPosAngle(oid, "FIXED", 0) *> stateAndTransitions(pid, oid)
    yield
      assertEquals(unfixed, (Undefined, List(Inactive)))
      assertEquals(refixed, (Ready, List(Inactive, Defined)))
