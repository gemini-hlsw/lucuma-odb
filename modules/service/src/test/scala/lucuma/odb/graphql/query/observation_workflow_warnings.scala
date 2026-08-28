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
import lucuma.core.enums.SkyBackground
import lucuma.core.math.*
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import lucuma.itc.SignalToNoiseAt
import lucuma.odb.graphql.mutation.UpdateObservationsOps

class observation_workflow_warnings
  extends ExecutionTestSupportForGmos
     with UpdateObservationsOps {

  def workflowQuery(oids: Observation.Id*) =
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

  def workflowQueryResult(wfs: CalculatedValue[ObservationWorkflow]*): Json =
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

  def approveConfigurationRequestHack(req: ConfigurationRequest.Id): IO[Unit] =
    import skunk.syntax.all.*
    import lucuma.odb.util.Codecs.configuration_request_id
    session.use: s =>
      s.prepareR(sql"update t_configuration_request set c_status = 'approved' where c_configuration_request_id = $configuration_request_id".command).use: ps =>
        ps.execute(req).void

  test("conditions probability < 10%") {

    // better, but not best
    def updateConditions(oid: Observation.Id) =
      updateObservationConditions(
        pi,
        oid,
        CloudExtinction.Preset.PointOne,
        ImageQuality.Preset.OnePointZero,
        SkyBackground.Darkest
      )

    val setup: IO[Observation.Id] =
      for
        cfp <- createGeminiCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        tid <- createTargetAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- updateConditions(oid)
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
        _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      yield oid

    setup.flatMap: oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Defined,
              List(ObservationWorkflowState.Inactive),
              List(ObservationValidation.genericWarning("Conditions likelihood is 9%."))
            )
          )
        ).asRight
      )
  }

  // sorry
  var HACK_ITC = false
  override def fakeSignalToNoiseAt(w: Wavelength): SignalToNoiseAt =
    if (HACK_ITC) then
      SignalToNoiseAt(
        w,
        SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1)),
        TotalSN(SignalToNoise.unsafeFromBigDecimalExact(2))
      )
    else super.fakeSignalToNoiseAt(w)

  test("total s/n < 3") {

    val setup: IO[Observation.Id] =
      for
        cfp <- createGeminiCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        tid <- createTargetAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- IO { HACK_ITC = true }
        _   <- computeItcResultAs(pi, oid)
        _   <- IO { HACK_ITC = false }
        _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      yield oid

    setup.flatMap: oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Defined,
              List(ObservationWorkflowState.Inactive),
              List(ObservationValidation.genericWarning("Total S/N  is 2.000 (min. 3.000 recommended)"))
            )
          )
        ).asRight
      )

  }

  val createPhaseTwoObservationWithWarnings: IO[Observation.Id] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "Foo")
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
      _   <- IO { HACK_ITC = true }
      _   <- computeItcResultAs(pi, oid)
      _   <- IO { HACK_ITC = false }
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
    yield oid

  test("Phase 2 warning should switch READY transition to FOR_REVIEW") {
    createPhaseTwoObservationWithWarnings.flatMap: oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Defined,
              List(
                ObservationWorkflowState.Inactive, 
                ObservationWorkflowState.ForReview,
              ),
              List(ObservationValidation.genericWarning("Total S/N  is 2.000 (min. 3.000 recommended)"))
            )
          )
        ).asRight
      )
  }

}
