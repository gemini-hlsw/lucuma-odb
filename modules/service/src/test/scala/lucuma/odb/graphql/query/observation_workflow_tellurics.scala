// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import lucuma.odb.graphql.mutation.UpdateObservationsOps
class observation_workflow_tellurics
  extends ExecutionTestSupportForGmos
     with UpdateObservationsOps {

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

  def updateCloudExtinctionAs(user: User, oid: Observation.Id, cloudExtinction: CloudExtinction.Preset): IO[Unit] =
    updateObservation(user, oid,
      update = s"""
        constraintSet: {
          cloudExtinction: ${cloudExtinction.tag.toUpperCase()}
        }
      """,
      query = """
        observations {
          id
        }
      """,
      expected = Right(json"""
        {
          "updateObservations": {
            "observations": [
              {
                "id": $oid
              }
            ]
          }
        }
      """)
    )

  def approveConfigurationRequestHack(req: ConfigurationRequest.Id): IO[Unit] =
    import skunk.syntax.all.*
    import lucuma.odb.util.Codecs.configuration_request_id
    session.use: s =>
      s.prepareR(sql"update t_configuration_request set c_status = 'approved' where c_configuration_request_id = $configuration_request_id".command).use: ps =>
        ps.execute(req).void

  test("telluric workflow follows science up to ready"):
    val setup: IO[(Program.Id, Observation.Id, Observation.Id)] =
      for
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramWithNonPartnerPi(pi)
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- createTargetWithProfileAs(pi, pid)

        // science
        sci <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- updateCloudExtinctionAs(pi, sci, CloudExtinction.Preset.OnePointZero)  // ask for poor conditions
        _   <- createConfigurationRequestAs(pi, sci).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, sci)

        // Cal
        cal <- createObservationAs(pi, pid, tid)

        // Group, and *then* set cal role
        gid <- createGroupAs(pi, pid, None)
        _   <- moveObservationAs(pi, sci, Some(gid))
        _   <- moveObservationAs(pi, cal, Some(gid))
        _   <- setObservationCalibrationRole(List(cal), CalibrationRole.Telluric)

      yield (pid, sci, cal)

    setup.flatMap: (pid, sci, cal) =>
      // Initially we expect science to be defined, with transitions to inactive and ready
      runObscalcUpdateAs(serviceUser, pid, sci) >>
      expect(
        pi,
        workflowQuery(sci),
        expected = workflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Defined,
              List(ObservationWorkflowState.Inactive, ObservationWorkflowState.Ready),
              Nil
            )
          )
        ).asRight
      ) >>
      // Initially we expect cal to be defined, with transition to inactive
      runObscalcUpdateAs(serviceUser, pid, cal) >>
      expect(
        pi,
        workflowQuery(cal),
        expected = workflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Defined,
              Nil,
              Nil
            )
          )
        ).asRight
      ) >>
      // Set science to Ready and Telluric should follow
      setObservationWorkflowState(pi, sci, ObservationWorkflowState.Ready) >>
      runObscalcUpdateAs(serviceUser, pid, cal) >>
      expect(
        pi,
        workflowQuery(cal),
        expected = workflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Ready,
              Nil,
              Nil
            )
          )
        ).asRight
      ) >>
      // Set science to Inactive and Telluric should follow
      setObservationWorkflowState(pi, sci, ObservationWorkflowState.Inactive) >>
      runObscalcUpdateAs(serviceUser, pid, cal) >>
      expect(
        pi,
        workflowQuery(cal),
        expected = workflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Inactive,
              Nil,
              Nil
            )
          )
        ).asRight
      ) >>
      // Set science to Defined and Telluric should follow
      setObservationWorkflowState(pi, sci, ObservationWorkflowState.Defined) >>
      runObscalcUpdateAs(serviceUser, pid, cal) >>
      expect(
        pi,
        workflowQuery(cal),
        expected = workflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Defined,
              Nil,
              Nil
            )
          )
        ).asRight
      )


}
