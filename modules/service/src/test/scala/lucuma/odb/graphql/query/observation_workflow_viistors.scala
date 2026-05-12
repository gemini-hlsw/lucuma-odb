// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState

class observation_workflow_visitors extends OdbSuite with ObservingModeSetupOperations:

  val pi: User          = TestUsers.Standard.pi(1, 30)
  val serviceUser       = TestUsers.service(3)
  val staff: User       = TestUsers.Standard.staff(4, 33)

  val validUsers = List(pi, serviceUser, staff)

  private def workflowQuery(oids: Observation.Id*) =
    s"""
      query {
        observations(
          WHERE: {
            id: { IN: ${oids.asJson} }
          }
        ) {
          matches {
            workflow {
              state
              calculationState
              value {
                state
                validTransitions
                validationErrors {
                  code
                  messages
                }
              }
            }
          }
        }
      }
    """

  private def workflowQueryResult(wfs: CalculatedValue[ObservationWorkflow]*): Json =
    val embed = wfs.map: wf =>
      json"""
        {
          "workflow": {
            "state": ${wf.state},
            "calculationState": ${wf.state},
            "value": {
              "state": ${wf.value.state},
              "validTransitions": ${wf.value.validTransitions},
              "validationErrors": ${wf.value.validationErrors}
            }
          }
        }
      """
    json"""
      {
        "observations": {
          "matches": $embed
        }
      }
    """

  val mode = 
    """
      visitor: {
        mode: MAROON_X
        centralWavelength: {
          picometers: 123
        }
        scienceFov: {
          arcminutes: 1.23
        }
      }
    """

  test("visitor mode observations do not require generator params or ITC results."):

    val setup: IO[(Program.Id, Observation.Id)] =
      for
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramWithNonPartnerPi(pi)
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createObservationWithModeAs(pi, pid, List(tid), mode)
        _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      yield (pid, oid)

    setup.flatMap:
      case (pid, oid) =>
        expect(
          pi,
          workflowQuery(oid),
          expected = workflowQueryResult(
            CalculatedValue(
              CalculationState.Ready,
              ObservationWorkflow(
                ObservationWorkflowState.Unapproved,
                List(ObservationWorkflowState.Inactive),
                List(ObservationValidation.configurationRequestNotRequested)
              )
            )
          ).asRight
        )

  