// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.math.Angle
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.service.workflow.validator.PosAngleValidator

/**
 * MOS observations checked against `PosAngleValidator`: the position angle
 * must be fixed, and once a mask design is attached, fixed at the angle the
 * mask was designed for.
 */
class observation_workflow_mos extends ExecutionTestSupportForGmos with MosMaskSupport:

  private def gmosNorthMos(aid: Option[Attachment.Id]): String =
    s"""
      gmosNorthMos: {
        grating: R831_G5302
        filter: R_PRIME
        customMask: {
          slitWidth: CUSTOM_WIDTH_0_50
          ${aid.foldMap(a => s"attachmentId: ${a.asJson}")}
        }
        centralWavelength: { nanometers: 500 }
      }
    """

  private def flamingos2Mos: String =
    """
      flamingos2Mos: {
        disperser: R1200_JH
        filter: JH
        customMask: { slitWidth: CUSTOM_WIDTH_2_PIX }
      }
    """

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
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow {
              value {
                validationErrors {
                  code
                  messages
                }
              }
            }
          }
        }
      """
    ).map: json =>
      json
        .hcursor
        .downFields("observation", "workflow", "value", "validationErrors")
        .values
        .toList
        .flatten
        .map: v =>
          (
            v.hcursor.downField("code").require[ObservationValidationCode],
            v.hcursor.downField("messages").require[List[String]]
          )
        .filter: (code, _) =>
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

  test("a new GMOS MOS observation is fixed and valid"):
    for
      (pid, tid) <- setup
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMos(none))
      _          <- expectValidations(pid, oid)
    yield ()

  test("a GMOS MOS observation with an unfixed position angle is an error"):
    for
      (pid, tid) <- setup
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMos(none))
      _          <- setPosAngle(oid, "AVERAGE_PARALLACTIC", 0)
      _          <- expectValidations(pid, oid, error(PosAngleValidator.ModeMessage))
    yield ()

  test("allow flip is not fixed either"):
    for
      (pid, tid) <- setup
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMos(none))
      _          <- setPosAngle(oid, "ALLOW_FLIP", 0)
      _          <- expectValidations(pid, oid, error(PosAngleValidator.ModeMessage))
    yield ()

  test("a Flamingos-2 MOS observation with an unfixed position angle is an error"):
    for
      (pid, tid) <- setup
      oid        <- createObservationWithModeAs(pi, pid, List(tid), flamingos2Mos)
      _          <- setPosAngle(oid, "UNBOUNDED", 0)
      _          <- expectValidations(pid, oid, error(PosAngleValidator.ModeMessage))
    yield ()

  test("a fixed angle that differs from the mask's angle is a warning"):
    for
      (pid, tid) <- setup
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth, Angle.fromDoubleDegrees(45.0).some)
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMos(aid.some))
      _          <- setPosAngle(oid, "FIXED", 30)
      _          <- expectValidations(pid, oid, warning(PosAngleValidator.angleMessage(Angle.fromDoubleDegrees(45.0), Angle.fromDoubleDegrees(30.0))))
    yield ()

  test("a fixed angle that matches the mask's angle is valid"):
    for
      (pid, tid) <- setup
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-02_ODF.fits", Instrument.GmosNorth, Angle.fromDoubleDegrees(45.0).some)
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMos(aid.some))
      _          <- setPosAngle(oid, "FIXED", 45)
      _          <- expectValidations(pid, oid)
    yield ()

  test("a mask without a parsed design produces no angle warning"):
    for
      (pid, tid) <- setup
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-03_ODF.fits", Instrument.GmosNorth)
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMos(aid.some))
      _          <- setPosAngle(oid, "FIXED", 30)
      _          <- expectValidations(pid, oid)
    yield ()

  test("an unfixed observation with a mask reports only the mode error"):
    for
      (pid, tid) <- setup
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-04_ODF.fits", Instrument.GmosNorth, Angle.fromDoubleDegrees(45.0).some)
      oid        <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMos(aid.some))
      _          <- setPosAngle(oid, "UNBOUNDED", 0)
      _          <- expectValidations(pid, oid, error(PosAngleValidator.ModeMessage))
    yield ()

  test("a long slit observation may use any position angle mode"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setPosAngle(oid, "AVERAGE_PARALLACTIC", 0)
      _          <- expectValidations(pid, oid)
    yield ()

  // Workflow transitions.  Reaching Defined with Ready on offer needs an
  // accepted proposal, an approved configuration request, an ITC result and a
  // science band, so these set up a full program around one GMOS North MOS
  // observation.

  private def stateAndTransitions(pid: Program.Id, oid: Observation.Id): IO[(ObservationWorkflowState, List[ObservationWorkflowState])] =
    for
      _  <- runObscalcUpdate(pid, oid)
      js <- query(pi, s"""
              query {
                observation(observationId: ${oid.asJson}) {
                  workflow { value { state validTransitions } }
                }
              }
            """)
    yield
      val c = js.hcursor.downFields("observation", "workflow", "value")
      (c.downField("state").require[ObservationWorkflowState], c.downField("validTransitions").require[List[ObservationWorkflowState]])

  private def acceptedMosObservation(maskAngle: Option[Angle]): IO[(Program.Id, Observation.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      aid <- maskAngle.traverse(a => insertMosMaskAttachment(pid, "GN2025AQ001-05_ODF.fits", Instrument.GmosNorth, a.some))
      _   <- addProposal(pi, pid, cfp.some, None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMos(aid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- computeItcResultAs(pi, oid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band1, 4.hourTimeSpan)))
      _   <- setScienceBandAs(pi, oid, ScienceBand.Band1.some)
    yield (pid, oid)

  import ObservationWorkflowState.*

  test("transitions: a fixed MOS observation is Defined and may become Ready"):
    for
      (pid, oid) <- acceptedMosObservation(none)
      st         <- stateAndTransitions(pid, oid)
    yield assertEquals(st, (Defined, List(Inactive, Ready)))

  test("transitions: an unfixed MOS observation is Undefined and may only go Inactive"):
    for
      (pid, oid) <- acceptedMosObservation(none)
      _          <- setPosAngle(oid, "AVERAGE_PARALLACTIC", 0)
      st         <- stateAndTransitions(pid, oid)
    yield assertEquals(st, (Undefined, List(Inactive)))

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
