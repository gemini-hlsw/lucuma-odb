// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
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

  val TooShort: String =
    "1 science step has an exposure time below the 1 s minimum for GMOS North (shortest 0.5 s)."

  def replaceSequence(oid: Observation.Id, sequenceType: SequenceType, seconds: BigDecimal): IO[Unit] =
    val step        = stepInput(GmosNorthFilter.GPrime).replace("seconds: 20", s"seconds: $seconds")
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
        .filter(_.hcursor.downField("code").require[ObservationValidationCode] === ObservationValidationCode.ConfigurationError)
        .flatMap(_.hcursor.downField("messages").require[List[String]])

  // An accepted observation with an approved configuration, so it may be Ready.
  val setup: IO[(Program.Id, Observation.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "Foo")
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- runObscalcUpdate(pid, oid)
    yield (pid, oid)

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
      _          <- assertIO(configurationErrors(oid), List(TooShort))
    yield ()

  test("an edited acquisition step below the minimum is an error"):
    for
      (pid, oid) <- setup
      _          <- replaceSequence(oid, SequenceType.Acquisition, 0.5)
      _          <- runObscalcUpdate(pid, oid)
      _          <- assertIO(configurationErrors(oid), List(TooShort.replace("science", "acquisition")))
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
