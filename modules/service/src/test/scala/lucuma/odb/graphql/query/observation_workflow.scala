// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package query

import cats.effect.IO
import cats.syntax.all.*
import clue.ResponseException
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.SequenceType
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.CallForProposals
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.mutation.UpdateConstraintSetOps
import lucuma.odb.json.all.transport.given
import lucuma.odb.service.ObservationService
import lucuma.odb.service.ObservationWorkflowService

class observation_workflow 
  extends ExecutionTestSupportForGmos
     with UpdateConstraintSetOps {

  // ra and dec values in degrees
  val RaStart = 90
  val RaCenter = 180
  val RaEnd = 270
  val RaStartWrap =  RaEnd
  val RaCenterWrap = 1
  val RaEndWrap = RaStart
  val DecStart = 0
  val DecCenter = 25
  val DecEnd = 45
  val Limits = (RaStart, RaEnd, DecStart, DecEnd)
  val WrappedLimits = (RaStartWrap, RaEndWrap, DecStart, DecEnd)

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
              validTransitions
              validationErrors {
                code
                messages
              }
            }
          }
        }
      }
    """

  def calculatedWorkflowQuery(oids: Observation.Id*) =
    s"""
      query {
        observations(
          WHERE: {
            id: { IN: ${oids.asJson} }
          }
        ) {
          matches {
            calculatedWorkflow {
              state
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

  def workflowQueryResult(wfs: ObservationWorkflow*): Json =
    val embed = wfs.map: wf =>
      json"""
        {
          "workflow": {
            "state": ${wf.state},
            "validTransitions": ${wf.validTransitions},
            "validationErrors": ${wf.validationErrors}
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

  def calculatedWorkflowQueryResult(wfs: CalculatedValue[ObservationWorkflow]*): Json =
    val embed = wfs.map: wf =>
      json"""
        {
          "calculatedWorkflow": {
            "state": ${wf.state},
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

  // only use this if you have instruments, limits or both. Otherwise use createCallForProposalsAs(...)
  def createCfp(
    instruments: List[Instrument] = List.empty,
    limits: Option[(Int, Int, Int, Int)] = none
  ): IO[CallForProposals.Id] =
    val inStr = 
      if (instruments.isEmpty) ""
      else s"instruments: [${instruments.map(_.tag.toScreamingSnakeCase).mkString(",")}]\n"
    val limitStr = limits.fold("") { (raStart, raEnd, decStart, decEnd) =>
      s"""
        coordinateLimits: {
          north: {
            raStart: { degrees: $raStart }
            raEnd: { degrees: $raEnd }
            decStart: { degrees: $decStart }
            decEnd: { degrees: $decEnd }
          }
          south: {
            raStart: { degrees: $raStart }
            raEnd: { degrees: $raEnd }
            decStart: { degrees: $decStart }
            decEnd: { degrees: $decEnd }
          }
        }
      """
    }
    createCallForProposalsAs(staff, other = (inStr + limitStr).some)

  def setTargetCoords(tid: Target.Id, raHours: Int, decHours: Int): IO[Unit] =
    query(
      user = pi,
      query = s"""
        mutation {
          updateTargets(input: {
            SET: {
              sidereal: {
                ra: { degrees: $raHours }
                dec: { degrees: $decHours }
                epoch: "J2000.00"
              }
            }
            WHERE: {
              id: { EQ: "$tid" }
            }
          }) {
            targets {
              id
            }
          }
        }
      """
    ).void

  def setObservationExplicitBase(oid: Observation.Id, raHours: Int, decHours: Int): IO[Unit] =
    query(
      user = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              targetEnvironment: {
                explicitBase: {
                  ra: { degrees: $raHours }
                  dec: { degrees: $decHours }
                }
              }
            },
            WHERE: {
              id: { EQ: ${oid.asJson} }
            }
          }) {
            observations {
              id
            }
          }
        }
      """
    ).void

  test("no target") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        oid <- createObservationAs(pi, pid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.configuration(ObservationService.MissingDataMsg(none, "target")))
          )
        ).asRight
      )
    }
  }

  test("no target - calculated"):
    val setup: IO[Observation.Id] =
      for
        pid <- createProgramAs(pi)
        oid <- createObservationAs(pi, pid)
        _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      yield oid

    setup.flatMap: oid =>
      expect(
        pi,
        calculatedWorkflowQuery(oid),
        expected = calculatedWorkflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Undefined,
              List(ObservationWorkflowState.Inactive),
              List(ObservationValidation.configuration(ObservationService.MissingDataMsg(none, "target")))
            )
          )
        ).asRight
      )

  testWithTargetTypes("no observing mode") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        tid <- mkTarget(pi, pid)
        oid <- createObservationAs(pi, pid, tid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.configuration(ObservationService.MissingDataMsg(none, "observing mode")))
          )
        ).asRight
      )
    }
  }

  test("missing target info") {
    val setup: IO[(Target.Id, Observation.Id)] =
      for {
        pid <- createProgramAs(pi)
        tid <- createIncompleteTargetAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield (tid, oid)
    setup.flatMap { (_, oid) =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.configuration("Missing brightness measure"))
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("otherwise ok, but no itc results in cache") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.itc("ITC results are not present."))
          )
        ).asRight
      )
    }
  }

  // temporary, until this is doable via graphql
  def approveConfigurationRequestHack(req: ConfigurationRequest.Id): IO[Unit] =
    import skunk.syntax.all.*
    import lucuma.odb.util.Codecs.configuration_request_id
    session.use: s =>
      s.prepareR(sql"update t_configuration_request set c_status = 'approved' where c_configuration_request_id = $configuration_request_id".command).use: ps =>
        ps.execute(req).void

  def denyConfigurationRequestHack(req: ConfigurationRequest.Id): IO[Unit] =
    import skunk.syntax.all.*
    import lucuma.odb.util.Codecs.configuration_request_id
    session.use: s =>
      s.prepareR(sql"update t_configuration_request set c_status = 'denied' where c_configuration_request_id = $configuration_request_id".command).use: ps =>
        ps.execute(req).void

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

  test("missing target info, invalid instrument") {
    val setup: IO[(Target.Id, Observation.Id)] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosSouth))
        _   <- addProposal(pi, pid, cid.some)
        tid <- createIncompleteTargetAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield (tid, oid)
    setup.flatMap { (_, oid) =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(
              ObservationValidation.configuration("Missing brightness measure"),
              ObservationValidation.callForProposals(ObservationService.InvalidInstrumentMsg(Instrument.GmosNorth))
            )
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("no validations") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(
            ObservationWorkflowState.Defined,
            List(ObservationWorkflowState.Inactive),
            Nil
          )
        ).asRight
      )
    }
  }
    
  testWithTargetTypes("valid instrument") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth))
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(
            ObservationWorkflowState.Defined,
            List(ObservationWorkflowState.Inactive),
            Nil
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("invalid instrument") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosSouth))
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(
              ObservationValidation.callForProposals(ObservationService.InvalidInstrumentMsg(Instrument.GmosNorth))
            )
          )
        ).asRight
      )
    }
  }
  testWithTargetTypes("explicit base within limits") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))     
         _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      List((RaStart, DecStart), (RaEnd, DecEnd), (RaStart, DecEnd), (RaCenter, DecCenter)).traverse { (ra, dec) =>
        setObservationExplicitBase(oid, ra, dec) >>
        createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack) >>
          expect(
            pi,
            workflowQuery(oid),
            expected = workflowQueryResult(
              ObservationWorkflow(          
                ObservationWorkflowState.Defined,
                List(ObservationWorkflowState.Inactive),
                Nil
              )
            ).asRight
          )
      }
    }
  }

  testWithTargetTypes("explicit base within limits, CfP with wrapped RA limits") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = WrappedLimits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      List((RaStartWrap, DecStart), (RaEndWrap, DecEnd), (RaStartWrap, DecEnd), (RaCenterWrap, DecCenter)).traverse { (ra, dec) =>
        setObservationExplicitBase(oid, ra, dec) >>
          createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack) >>
          expect(
            pi,
            workflowQuery(oid),
            expected = workflowQueryResult(
              ObservationWorkflow(          
                ObservationWorkflowState.Defined,
                List(ObservationWorkflowState.Inactive),
                Nil
              )
            ).asRight
          )
      }
    }
  }

  testWithTargetTypes("explicit base outside limits") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      List((RaStart - 1, DecStart), (RaEnd, DecEnd + 1), (RaCenterWrap, DecCenter)).traverse { (ra, dec) =>
        setObservationExplicitBase(oid, ra, dec) >>
          expect(
            pi,
            workflowQuery(oid),
            expected = workflowQueryResult(
              ObservationWorkflow(          
                ObservationWorkflowState.Undefined,
                List(ObservationWorkflowState.Inactive),
                List(ObservationValidation.callForProposals(ObservationWorkflowService.Messages.CoordinatesOutOfRange))
              )
            ).asRight
          )
      }
    }
  }

  testWithTargetTypes("explicit base outside limits, CfP with wrapped RA limits") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = WrappedLimits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      List((RaStartWrap, DecStart - 1), (RaEndWrap + 1, DecEnd), (RaStartWrap - 1, DecEnd), (RaCenter, DecCenter)).traverse { (ra, dec) =>
        setObservationExplicitBase(oid, ra, dec) >>
          expect(
            pi,
            workflowQuery(oid),
            expected = workflowQueryResult(
              ObservationWorkflow(          
                ObservationWorkflowState.Undefined,
                List(ObservationWorkflowState.Inactive),
                List(ObservationValidation.callForProposals(ObservationWorkflowService.Messages.CoordinatesOutOfRange))
              )
            ).asRight
          )
      }
    }
  }

  testWithTargetTypes("asterism within limits, valid instrument") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        _   <- setTargetCoords(tid, RaCenter, DecCenter)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Defined,
            List(ObservationWorkflowState.Inactive),
            Nil
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("asterism outside limits, valid instrument") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        _   <- setTargetCoords(tid, RaStart - 20, DecCenter)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.callForProposals(ObservationWorkflowService.Messages.CoordinatesOutOfRange))
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("asterism outside limits, invalid instrument") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        _   <- setTargetCoords(tid, RaStart - 20, DecCenter)
        oid <- createGmosSouthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(
              ObservationValidation.callForProposals(
                ObservationService.InvalidInstrumentMsg(Instrument.GmosSouth),
                ObservationWorkflowService.Messages.CoordinatesOutOfRange
              )
            )
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("invalid band") { (_, mkTarget) =>
    val allocations = List(
      AllocationInput(TimeAccountingCategory.US, ScienceBand.Band1, 1.hourTimeSpan),
      AllocationInput(TimeAccountingCategory.CA, ScienceBand.Band2, 4.hourTimeSpan)
    )

    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        _   <- setAllocationsAs(staff, pid, allocations)
        tid <- mkTarget(pi, pid)
        oid <- createGmosSouthLongSlitObservationAs(pi, pid, List(tid))
        _   <- computeItcResultAs(pi, oid)
        _   <- setScienceBandAs(pi, oid, ScienceBand.Band1.some)
        _   <- setAllocationsAs(staff, pid, allocations.tail).intercept[ResponseException[Json]].void
      } yield oid

    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(
              ObservationValidation.configuration(
                ObservationService.InvalidScienceBandMsg(ScienceBand.Band1)
              )
            )
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("no configuration request checks if proposal is not approved") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Defined,
            List(ObservationWorkflowState.Inactive),
            Nil
          )
        ).asRight // no warnings!
      )
    }
  }

  testWithTargetTypes("no configuration request") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Unapproved,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.configurationRequestNotRequested)
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("unapproved configuration request") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid)
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Unapproved,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.configurationRequestPending)
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("denied configuration request") { (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(denyConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Unapproved,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.configurationRequestDenied)
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("approved configuration request") { (tt, mkTarget) =>
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Defined,
            tt match
              case TargetType.Sidereal    => List(ObservationWorkflowState.Inactive, ObservationWorkflowState.Ready)
              case TargetType.Nonsidereal => ???
              case TargetType.Opportunity => List(ObservationWorkflowState.Inactive)             
            ,
            Nil
          )
        ).asRight
      )
    }
  }

  test("calibrations are not validated and are immediately Ready") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        oid <- createObservationAs(pi, pid) // is missing target
        _   <- setObservationCalibratioRole(oid, CalibrationRole.SpectroPhotometric.some)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Ready,
            Nil,
            Nil
          )
        ).asRight
      )
    }
  }

  test("observations in engineering programs are not validated and are immediately Defined") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        _   <- setProgramReference(staff, pid, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
        oid <- createObservationAs(pi, pid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Defined,
            List(ObservationWorkflowState.Inactive, ObservationWorkflowState.Ready),
            Nil
          )
        ).asRight
      )
    }
  }

  testWithTargetTypes("approved configuration request AND asterism outside limits") { (tt, mkTarget) =>

    val oid1: IO[Observation.Id]  =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi, "Foo")
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
      } yield oid

    val oid2: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- mkTarget(pi, pid)
        _   <- setTargetCoords(tid, RaStart - 20, DecCenter)
        oid <- createGmosSouthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid

    (oid1, oid2).tupled.flatMap { (oid1, oid2) =>
      expect(
        pi,
        workflowQuery(oid1, oid2),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Defined,
            tt match
              case TargetType.Sidereal    => List(ObservationWorkflowState.Inactive, ObservationWorkflowState.Ready)
              case TargetType.Nonsidereal => ???
              case TargetType.Opportunity => List(ObservationWorkflowState.Inactive)             
            ,
            Nil
          ),
          ObservationWorkflow(          
            ObservationWorkflowState.Undefined,
            List(ObservationWorkflowState.Inactive),
            List(
              ObservationValidation.callForProposals(
                ObservationService.InvalidInstrumentMsg(Instrument.GmosSouth),
                ObservationWorkflowService.Messages.CoordinatesOutOfRange
              ),
            )
          )
        ).asRight
      )

    }

  }


  testWithTargetTypes("observation should move back to Unapproved if better conditions are requested") { (tt, mkTarget) =>

    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi)
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- mkTarget(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- updateCloudExtinctionAs(pi, oid, CloudExtinction.Preset.OnePointFive)  // ask for poor conditions
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
      } yield oid

    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Defined,
            tt match
              case TargetType.Sidereal    => List(ObservationWorkflowState.Inactive, ObservationWorkflowState.Ready)
              case TargetType.Nonsidereal => ???
              case TargetType.Opportunity => List(ObservationWorkflowState.Inactive)             
            ,
            Nil
          )
        ).asRight
      ) >>
      updateCloudExtinctionAs(pi, oid, CloudExtinction.Preset.PointFive) >>  // ask for better conditions
      computeItcResultAs(pi, oid) >> // recompute ITC
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Unapproved,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.configurationRequestNotRequested)
          )
        ).asRight
      )
    }

  }

  test("sc-4269 observation should move back to Unapproved from Ready if better conditions are requested") {

    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi)
        _   <- addProposal(pi, pid, Some(cfp), None)
        _   <- addPartnerSplits(pi, pid)
        _   <- addCoisAs(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _    <- updateCloudExtinctionAs(pi, oid, CloudExtinction.Preset.OnePointFive)  // ask for poor conditions
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResultAs(pi, oid)
        _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready)
      } yield oid

    setup.flatMap { oid =>
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Ready,
            List(ObservationWorkflowState.Inactive, ObservationWorkflowState.Defined),  // intially approved
            Nil
          )
        ).asRight
      ) >>
      updateCloudExtinctionAs(pi, oid, CloudExtinction.Preset.PointFive) >>  // ask for better conditions
      computeItcResultAs(pi, oid) >> // recompute ITC
      expect(
        pi,
        workflowQuery(oid),
        expected = workflowQueryResult(
          ObservationWorkflow(          
            ObservationWorkflowState.Unapproved,
            List(ObservationWorkflowState.Inactive),
            List(ObservationValidation.configurationRequestNotRequested)
          )
        ).asRight
      )
    }

  }

  testWithTargetTypes("calculated: default workflow"): (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- mkTarget(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        // no obscalc update, so it remains in pending state
      yield o

    setup.flatMap: oid =>
      expect(
        pi,
        calculatedWorkflowQuery(oid),
        expected = calculatedWorkflowQueryResult(
          CalculatedValue(
            CalculationState.Pending,
            ObservationWorkflow(
              ObservationWorkflowState.Undefined,
              List(ObservationWorkflowState.Inactive),
              Nil
            )
          )
        ).asRight
      )

  val OngoingSetup: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
      _ <- addEndStepEvent(s)
      _ <- runObscalcUpdate(p, o)
    yield o

  test("calculated: ongoing"):
    OngoingSetup.flatMap: oid =>
      expect(
        pi,
        calculatedWorkflowQuery(oid),
        expected = calculatedWorkflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Ongoing,
              List(ObservationWorkflowState.Inactive, ObservationWorkflowState.Completed),
              Nil
            )
          )
        ).asRight
      )

  val MissingTargetInvalidInstrumentSetup: IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      c <- createCfp(List(Instrument.GmosSouth))
      _ <- addProposal(pi, p, c.some)
      t <- createIncompleteTargetAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- runObscalcUpdate(p, o)
    yield o

  test("calculated: missing target info, invalid instrument"):
    MissingTargetInvalidInstrumentSetup.flatMap: oid =>
      expect(
        pi,
        calculatedWorkflowQuery(oid),
        expected = calculatedWorkflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Undefined,
              List(ObservationWorkflowState.Inactive),
              List(
                ObservationValidation.configuration("Missing brightness measure"),
                ObservationValidation.callForProposals(ObservationService.InvalidInstrumentMsg(Instrument.GmosNorth))
              )
            )
          )
        ).asRight
      )

  private def expectWhere(all: Observation.Id*)(
    where:    String,
    expected: List[Observation.Id]
  ): IO[Unit] =
    val matches = expected.map(id => Json.obj("id" -> id.asJson)).asJson

    expect(
      user  = pi,
      query = s"""
        query {
          observations(WHERE: {
            id: { IN: ${all.map(id => s"\"$id\"").mkString("[", ", ", "]")} }
            workflow: { $where }
          }) {
            matches {
              id
            }
          }
        }
      """,
      expected = json"""
        {
          "observations": {
            "matches": $matches
          }
        }
      """.asRight
    )

  val PendingSetup: IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createIncompleteTargetAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield o

  test("calculated: where workflow"):
    for
      pending <- PendingSetup
      ongoing <- OngoingSetup
      invalid <- MissingTargetInvalidInstrumentSetup
      matches  = expectWhere(pending, ongoing, invalid)
      _       <- matches("workflowState: { GTE: READY }", List(ongoing))
      _       <- matches("workflowState: { EQ: UNDEFINED }", List(pending, invalid))
      _       <- matches("calculationState: { EQ: READY }", List(ongoing, invalid))
      _       <- matches("calculationState: { LT: READY }", List(pending))
      _       <- matches("IS_NULL: false", List(pending, ongoing, invalid))
    yield ()

  // Note, this differs from the "normal" workflow calculation because that
  // does not do the ITC query whereas this version will do it before the
  // workflow is computed.
  testWithTargetTypes("calculated: otherwise ok, but no itc results in cache"): (_, mkTarget) =>
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- mkTarget(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        pi,
        calculatedWorkflowQuery(oid),
        expected = calculatedWorkflowQueryResult(
          CalculatedValue(
            CalculationState.Ready,
            ObservationWorkflow(
              ObservationWorkflowState.Defined,
              List(ObservationWorkflowState.Inactive),
              Nil
            )
          )
        ).asRight
      )

}
