// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.CallForProposals
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.mutation.UpdateConstraintSetOps
import lucuma.odb.service.ObservationService

class observation_workflow 
  extends OdbSuite
     with ObservingModeSetupOperations 
     with UpdateConstraintSetOps {

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(3, 103)
  
  override def validUsers: List[User] = List(pi, staff)

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

  def validationQuery(oid: Observation.Id) = 
    s"""
      query {
        observation(observationId: "$oid") {
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
    """

  def itcQuery(oid: Observation.Id) = 
    s"""
      query {
        observation(observationId: "$oid") {
          itc {
            science {
              selected {
                targetId
              }
            }
          }
        }
      }
    """

  // Load up the cache with an ITC result
  def computeItcResult(oid: Observation.Id): IO[Unit] =
    query(pi, itcQuery(oid)).void

  def queryResult(state: ObservationWorkflowState, states: List[ObservationWorkflowState], errs: ObservationValidation*): Json =
    json"""
      {
        "observation": {
          "workflow": {
            "state": $state,
            "validTransitions": $states,
            "validationErrors": $errs
          }
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
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.configuration(ObservationService.MissingDataMsg(none, "target"))
        ).asRight
      )
    }
  }

  test("no observing mode") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        tid <- createTargetAs(pi, pid)
        oid <- createObservationAs(pi, pid, tid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.configuration(ObservationService.MissingDataMsg(none, "observing mode"))
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
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.configuration(
            "Missing brightness measure, radial velocity"
          )
        ).asRight
      )
    }
  }

  test("otherwise ok, but no itc results in cache") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.itc("ITC results are not present.")
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

  test("no validations") {
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi)
        _   <- addProposal(pi, pid, Some(cfp), None, "Foo")
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Defined,
          List(ObservationWorkflowState.Inactive),
        ).asRight
      )
    }
  }
    
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
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.configuration(
            "Missing brightness measure, radial velocity"
          ),
          ObservationValidation.callForProposals(
            ObservationService.InvalidInstrumentMsg(Instrument.GmosNorth)
          )
        ).asRight
      )
    }
  }

  test("valid instrument") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth))
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Defined,
          List(ObservationWorkflowState.Inactive),
        ).asRight
      )
    }
  }

  test("invalid instrument") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosSouth))
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.callForProposals(
            ObservationService.InvalidInstrumentMsg(Instrument.GmosNorth)
          )
        ).asRight
      )
    }
  }

  test("explicit base within limits") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))     
         _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      List((RaStart, DecStart), (RaEnd, DecEnd), (RaStart, DecEnd), (RaCenter, DecCenter)).traverse { (ra, dec) =>
        setObservationExplicitBase(oid, ra, dec) >>
        createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack) >>
          expect(
            pi,
            validationQuery(oid),
            expected = queryResult(
              ObservationWorkflowState.Defined,
              List(ObservationWorkflowState.Inactive),
            ).asRight
          )
      }
    }
  }

  test("explicit base within limits, CfP with wrapped RA limits") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = WrappedLimits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      List((RaStartWrap, DecStart), (RaEndWrap, DecEnd), (RaStartWrap, DecEnd), (RaCenterWrap, DecCenter)).traverse { (ra, dec) =>
        setObservationExplicitBase(oid, ra, dec) >>
          createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack) >>
          expect(
            pi,
            validationQuery(oid),
            expected = queryResult(
              ObservationWorkflowState.Defined,
              List(ObservationWorkflowState.Inactive),
            ).asRight
          )
      }
    }
  }

  test("explicit base outside limits") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      List((RaStart - 1, DecStart), (RaEnd, DecEnd + 1), (RaCenterWrap, DecCenter)).traverse { (ra, dec) =>
        setObservationExplicitBase(oid, ra, dec) >>
          expect(
            pi,
            validationQuery(oid),
            expected = queryResult(
              ObservationWorkflowState.Undefined,
              List(ObservationWorkflowState.Inactive),
              ObservationValidation.callForProposals(ObservationService.ExplicitBaseOutOfRangeMsg)
            ).asRight
          )
      }
    }
  }

  test("explicit base outside limits, CfP with wrapped RA limits") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = WrappedLimits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      List((RaStartWrap, DecStart - 1), (RaEndWrap + 1, DecEnd), (RaStartWrap - 1, DecEnd), (RaCenter, DecCenter)).traverse { (ra, dec) =>
        setObservationExplicitBase(oid, ra, dec) >>
          expect(
            pi,
            validationQuery(oid),
            expected = queryResult(
              ObservationWorkflowState.Undefined,
              List(ObservationWorkflowState.Inactive),
              ObservationValidation.callForProposals(ObservationService.ExplicitBaseOutOfRangeMsg)
            ).asRight
          )
      }
    }
  }

  test("asterism within limits, valid instrument") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        _   <- setTargetCoords(tid, RaCenter, DecCenter)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Defined,
          List(ObservationWorkflowState.Inactive),
        ).asRight
      )
    }
  }

  test("asterism outside limits, valid instrument") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        _   <- setTargetCoords(tid, RaStart - 20, DecCenter)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.callForProposals(ObservationService.AsterismOutOfRangeMsg)
        ).asRight
      )
    }
  }

  test("asterism outside limits, invalid instrument") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        cid <- createCfp(List(Instrument.GmosNorth), limits = Limits.some)
        _   <- addProposal(pi, pid, cid.some)
        tid <- createTargetWithProfileAs(pi, pid)
        _   <- setTargetCoords(tid, RaStart - 20, DecCenter)
        oid <- createGmosSouthLongSlitObservationAs(pi, pid, List(tid))
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.callForProposals(
            ObservationService.InvalidInstrumentMsg(Instrument.GmosSouth),
            ObservationService.AsterismOutOfRangeMsg
          )
        ).asRight
      )
    }
  }

  test("invalid band") {
    val allocations = List(
      AllocationInput(TimeAccountingCategory.US, ScienceBand.Band1, 1.hourTimeSpan),
      AllocationInput(TimeAccountingCategory.CA, ScienceBand.Band2, 4.hourTimeSpan)
    )

    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        _   <- setAllocationsAs(staff, pid, allocations)
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosSouthLongSlitObservationAs(pi, pid, List(tid))
        _   <- computeItcResult(oid)
        _   <- setScienceBandAs(pi, oid, ScienceBand.Band1.some)
        _   <- setAllocationsAs(staff, pid, allocations.tail).intercept[ResponseException[Json]].void
      } yield oid

    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Undefined,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.configuration(
            ObservationService.InvalidScienceBandMsg(ScienceBand.Band1)
          )
        ).asRight
      )
    }
  }

  test("no configuration request checks if proposal is not approved") {
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi)
        _   <- addProposal(pi, pid, Some(cfp), None, "Foo")
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Defined,
          List(ObservationWorkflowState.Inactive),
        ).asRight // no warnings!
      )
    }
  }

  test("no configuration request") {
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi)
        _   <- addProposal(pi, pid, Some(cfp), None, "Foo")
        _   <- addPartnerSplits(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Unapproved,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.configurationRequestNotRequested
        ).asRight
      )
    }
  }

  test("unapproved configuration request") {
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi)
        _   <- addProposal(pi, pid, Some(cfp), None, "Foo")
        _   <- addPartnerSplits(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid)
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Unapproved,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.configurationRequestPending
        ).asRight
      )
    }
  }

  test("denied configuration request") {
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi)
        _   <- addProposal(pi, pid, Some(cfp), None, "Foo")
        _   <- addPartnerSplits(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(denyConfigurationRequestHack)
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Unapproved,
          List(ObservationWorkflowState.Inactive),
          ObservationValidation.configurationRequestDenied
        ).asRight
      )
    }
  }

  test("approved configuration request") {
    val setup: IO[Observation.Id] =
      for {
        cfp <- createCallForProposalsAs(staff)
        pid <- createProgramAs(pi)
        _   <- addProposal(pi, pid, Some(cfp), None, "Foo")
        _   <- addPartnerSplits(pi, pid)
        _   <- setProposalStatus(staff, pid, "ACCEPTED")
        tid <- createTargetWithProfileAs(pi, pid)
        oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
        _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
        _   <- computeItcResult(oid)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Defined,
          List(ObservationWorkflowState.Inactive, ObservationWorkflowState.Ready),
        ).asRight
      )
    }
  }

  test("calibrations are not validated") {
    val setup: IO[Observation.Id] =
      for {
        pid <- createProgramAs(pi)
        oid <- createObservationAs(pi, pid) // is missing target
        _   <- setObservationCalibratioRole(oid, CalibrationRole.SpectroPhotometric.some)
      } yield oid
    setup.flatMap { oid =>
      expect(
        pi,
        validationQuery(oid),
        expected = queryResult(
          ObservationWorkflowState.Ready,
          Nil,
        ).asRight
      )
    }
  }

}
