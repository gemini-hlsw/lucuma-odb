// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SkyBackground
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.mutation.ReplaceGmosNorthSequenceOps
import lucuma.odb.graphql.mutation.UpdateObservationsOps

// The limits are checked on the steps themselves, so an edited sequence stands
// in for every way an exposure time reaches the sequence.  Generated sequences
// are covered by observation_workflow_gnirs.
class observation_workflow_exposureTime
  extends ExecutionTestSupportForGmos
     with ReplaceGmosNorthSequenceOps
     with UpdateObservationsOps:

  // 0.5 s is both too short and not a whole number of seconds.
  def tooShort(sequenceType: String): List[String] =
    List(
      s"${sequenceType.capitalize} sequence: Exposure times for GMOS North must be a whole number of seconds.",
      s"${sequenceType.capitalize} sequence: Exposure times for GMOS North must be at least 1 s."
    )

  def replaceSequence(oid: Observation.Id, sequenceType: SequenceType, seconds: BigDecimal, imaging: Boolean = false): IO[Unit] =
    val step0       = if imaging then imagingStepInput(GmosNorthFilter.RPrime) else stepInput(GmosNorthFilter.GPrime)
    val step        = step0.replace("seconds: 20", s"seconds: $seconds")
    val inputString = input(oid, sequenceType, atomInput("Foo", step))
    query(
      pi,
      s"""
        mutation {
          replaceGmosNorthSequence(input: $inputString) {
            sequence { description }
          }
        }
      """
    ).void

  def configurationErrors(oid: Observation.Id): IO[List[String]] =
    validations(oid, ObservationValidationCode.ConfigurationError)

  def exposureTimeWarnings(oid: Observation.Id): IO[List[String]] =
    validations(oid, ObservationValidationCode.ExposureTimeWarning)

  def setSkyBackground(oid: Observation.Id, sb: SkyBackground): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { constraintSet: { skyBackground: ${sb.tag.toUpperCase} } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  def validations(oid: Observation.Id, code: ObservationValidationCode): IO[List[String]] =
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
        .filter(_.hcursor.downField("code").require[ObservationValidationCode] === code)
        .flatMap(_.hcursor.downField("messages").require[List[String]])

  // An accepted observation with an approved configuration, so it may be Ready.
  def setupWith(create: (Program.Id, Target.Id) => IO[Observation.Id]): IO[(Program.Id, Observation.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "Foo")
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- create(pid, tid)
      _   <- createConfigurationRequestAs(pi, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- runObscalcUpdate(pid, oid)
    yield (pid, oid)

  val setup: IO[(Program.Id, Observation.Id)] =
    setupWith((pid, tid) => createGmosNorthLongSlitObservationAs(pi, pid, List(tid)))

  test("a generated sequence within the limits has no exposure time error"):
    for
      (_, oid) <- setup
      _        <- assertIO(configurationErrors(oid), Nil)
    yield ()

  test("an edited science step below the minimum is an error"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 0.5)
      _          <- runObscalcUpdate(pid, oid)
      _          <- assertIO(configurationErrors(oid), tooShort("science"))
    yield ()

  test("an edited acquisition step below the minimum is an error"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Acquisition, 0.5)
      _          <- runObscalcUpdate(pid, oid)
      _          <- assertIO(configurationErrors(oid), tooShort("acquisition"))
    yield ()

  test("an edited step at the minimum is fine"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 1)
      _          <- runObscalcUpdate(pid, oid)
      _          <- assertIO(configurationErrors(oid), Nil)
    yield ()

  test("fixing the edit clears the error"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 0.5)
      _          <- runObscalcUpdate(pid, oid)
      _          <- replaceSequence(oid, SequenceType.Science, 20)
      _          <- runObscalcUpdate(pid, oid)
      _          <- assertIO(configurationErrors(oid), Nil)
    yield ()

  // A requested transition is checked against a workflow computed without
  // generating the sequence, which must still see the stored issues.
  test("an observation with an exposure time error cannot be set Ready"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 0.5)
      _          <- runObscalcUpdate(pid, oid)
      _          <- interceptOdbError(setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready)):
                      case OdbError.InvalidWorkflowTransition(_, ObservationWorkflowState.Ready, _) => ()
    yield ()

  test("an observation without exposure time errors can be set Ready"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 20)
      _          <- runObscalcUpdate(pid, oid)
      _          <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready)
    yield ()

  // A transition is checked without generating the sequence, so until obscalc
  // has checked the latest edit it sees the violations obscalc last stored.
  // Obscalc's workflow is the guarantee: it catches up with the edit, and its
  // validation errors override a Ready user state.

  test("a bad edit accepted before obscalc checks it is demoted once it does"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 0.5)
      _          <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready)
      _          <- runObscalcUpdate(pid, oid)
      _          <- assertIO(queryObservationWorkflowState(pi, oid), ObservationWorkflowState.Undefined)
      _          <- assertIO(configurationErrors(oid), tooShort("science"))
    yield ()

  test("a fix refused before obscalc checks it is accepted once it does"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 0.5)
      _          <- runObscalcUpdate(pid, oid)
      _          <- replaceSequence(oid, SequenceType.Science, 20)
      _          <- interceptOdbError(setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready)):
                      case OdbError.InvalidWorkflowTransition(_, ObservationWorkflowState.Ready, _) => ()
      _          <- runObscalcUpdate(pid, oid)
      _          <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready)
    yield ()

  test("a transition sees a constraint edit that obscalc has not yet checked"):
    for
      (pid, oid) <- setupWith((pid, tid) => createGmosNorthImagingObservationAs(pi, pid, tid))
      _          <- setSkyBackground(oid, SkyBackground.Darkest)
      _          <- replaceSequence(oid, SequenceType.Science, 457, imaging = true)
      _          <- runObscalcUpdate(pid, oid)
      _          <- setSkyBackground(oid, SkyBackground.Bright)
      _          <- interceptOdbError(setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready)):
                      case OdbError.InvalidWorkflowTransition(_, ObservationWorkflowState.Ready, _) => ()
    yield ()

  test("the sequence digest reports the violations"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 0.5)
      _          <- runObscalcUpdate(pid, oid)
      _          <- expect(
                      pi,
                      s"""
                        query {
                          observation(observationId: ${oid.asJson}) {
                            execution {
                              digest {
                                value {
                                  acquisition { exposureTimeViolations { severity } }
                                  science {
                                    exposureTimeViolations {
                                      severity
                                      description
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      """,
                      json"""
                        {
                          "observation": {
                            "execution": {
                              "digest": {
                                "value": {
                                  "acquisition": { "exposureTimeViolations": [] },
                                  "science": {
                                    "exposureTimeViolations": [
                                      {
                                        "severity": "ERROR",
                                        "description": "Exposure times for GMOS North must be a whole number of seconds."
                                      },
                                      {
                                        "severity": "ERROR",
                                        "description": "Exposure times for GMOS North must be at least 1 s."
                                      }
                                    ]
                                  }
                                }
                              }
                            }
                          }
                        }
                      """.asRight
                    )
    yield ()

  test("a fractional GMOS exposure time is an error"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 1.5)
      _          <- runObscalcUpdate(pid, oid)
      _          <- assertIO(
                      configurationErrors(oid),
                      List("Science sequence: Exposure times for GMOS North must be a whole number of seconds.")
                    )
    yield ()

  test("a GMOS exposure longer than the cosmic ray limit is a warning"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Science, 1500)
      _          <- runObscalcUpdate(pid, oid)
      _          <- assertIO(configurationErrors(oid), Nil)
      _          <- assertIO(
                      exposureTimeWarnings(oid),
                      List("Science sequence: Exposure times above 1200 s are not recommended for GMOS North due to cosmic ray contamination.")
                    )
    yield ()

  // The saturation limit depends on the observation's sky background, which is
  // not part of the step.
  test("GMOS imaging saturation follows the observation's sky background"):
    val saturates =
      "Science sequence: Exposure times for GMOS North r imaging with 1x1 binning under a Bright sky must be at most 456 s due to sky background saturation."
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      _   <- setSkyBackground(oid, SkyBackground.Darkest)
      _   <- replaceSequence(oid, SequenceType.Science, 457, imaging = true)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(configurationErrors(oid), Nil)
      _   <- setSkyBackground(oid, SkyBackground.Bright)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(configurationErrors(oid), List(saturates))
    yield ()
