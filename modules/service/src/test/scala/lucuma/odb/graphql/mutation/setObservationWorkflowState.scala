// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class setObservationWorkflowState 
  extends OdbSuite
     with ObservingModeSetupOperations 
     with UpdateConstraintSetOps {

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(3, 103)
  
  override def validUsers: List[User] = List(pi, staff)

  // ra and dec values in degrees
  // val RaStart = 90
  // val RaCenter = 180
  // val RaEnd = 270
  // val RaStartWrap =  RaEnd
  // val RaCenterWrap = 1
  // val RaEndWrap = RaStart
  // val DecStart = 0
  // val DecCenter = 25
  // val DecEnd = 45
  // val Limits = (RaStart, RaEnd, DecStart, DecEnd)
  // val WrappedLimits = (RaStartWrap, RaEndWrap, DecStart, DecEnd)

  // def validationQuery(oid: Observation.Id) = 
  //   s"""
  //     query {
  //       observation(observationId: "$oid") {
  //         workflow {
  //           state
  //           validTransitions
  //           validationErrors {
  //             code
  //             messages
  //           }
  //         }
  //       }
  //     }
  //   """

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

  // def queryResult(state: ObservationWorkflowState, states: List[ObservationWorkflowState], errs: ObservationValidation*): Json =
  //   json"""
  //     {
  //       "observation": {
  //         "workflow": {
  //           "state": $state,
  //           "validTransitions": $states,
  //           "validationErrors": $errs
  //         }
  //       }
  //     }
  //   """ 

  // // only use this if you have instruments, limits or both. Otherwise use createCallForProposalsAs(...)
  // def createCfp(
  //   instruments: List[Instrument] = List.empty,
  //   limits: Option[(Int, Int, Int, Int)] = none
  // ): IO[CallForProposals.Id] =
  //   val inStr = 
  //     if (instruments.isEmpty) ""
  //     else s"instruments: [${instruments.map(_.tag.toScreamingSnakeCase).mkString(",")}]\n"
  //   val limitStr = limits.fold("") { (raStart, raEnd, decStart, decEnd) =>
  //     s"""
  //       coordinateLimits: {
  //         north: {
  //           raStart: { degrees: $raStart }
  //           raEnd: { degrees: $raEnd }
  //           decStart: { degrees: $decStart }
  //           decEnd: { degrees: $decEnd }
  //         }
  //         south: {
  //           raStart: { degrees: $raStart }
  //           raEnd: { degrees: $raEnd }
  //           decStart: { degrees: $decStart }
  //           decEnd: { degrees: $decEnd }
  //         }
  //       }
  //     """
  //   }
  //   createCallForProposalsAs(staff, other = (inStr + limitStr).some)

  // def setTargetCoords(tid: Target.Id, raHours: Int, decHours: Int): IO[Unit] =
  //   query(
  //     user = pi,
  //     query = s"""
  //       mutation {
  //         updateTargets(input: {
  //           SET: {
  //             sidereal: {
  //               ra: { degrees: $raHours }
  //               dec: { degrees: $decHours }
  //             }
  //           }
  //           WHERE: {
  //             id: { EQ: "$tid" }
  //           }
  //         }) {
  //           targets {
  //             id
  //           }
  //         }
  //       }
  //     """
  //   ).void

  // def setObservationExplicitBase(oid: Observation.Id, raHours: Int, decHours: Int): IO[Unit] =
  //   query(
  //     user = pi,
  //     query = s"""
  //       mutation {
  //         updateObservations(input: {
  //           SET: {
  //             targetEnvironment: {
  //               explicitBase: {
  //                 ra: { degrees: $raHours }
  //                 dec: { degrees: $decHours }
  //               }
  //             }
  //           },
  //           WHERE: {
  //             id: { EQ: ${oid.asJson} }
  //           }
  //         }) {
  //           observations {
  //             id
  //           }
  //         }
  //       }
  //     """
  //   ).void

  def queryObservationWorkflowState(oid: Observation.Id): IO[ObservationWorkflowState] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            workflow {
              state
            }
          }
        }
        """
    ).map: json =>
      json.hcursor.downFields("observation", "workflow", "state").require[ObservationWorkflowState]

  def setObservationWorkflowState(oid: Observation.Id, wfs: ObservationWorkflowState): IO[ObservationWorkflowState] =
    query(
      pi,
      s"""
        mutation {
          setObservationWorkflowState(input: {
            observationId: "$oid"
            state: ${wfs.tag.toUpperCase}  
          }) {
            state
          }
        }
        """
    ).map: json =>
      json.hcursor.downFields("setObservationWorkflowState", "state").require[ObservationWorkflowState]

  /** Test that we can change to this specified state, and then back. */
  def testTransition(oid: Observation.Id, state: ObservationWorkflowState): IO[Unit] =
    queryObservationWorkflowState(oid).flatMap: current =>
      setObservationWorkflowState(oid, state) >>
      setObservationWorkflowState(oid, current).void

  def testTransitions(oid: Observation.Id, allowedTransitions: ObservationWorkflowState*): IO[Unit] =
    val legal = allowedTransitions.toList
    val illegal = ObservationWorkflowState.values.toList.filterNot(legal.contains) 
    legal.traverse_(testTransition(oid, _)) >>
    illegal.traverse_ : state =>
      interceptOdbError(testTransition(oid, state)):
        case OdbError.InvalidWorkflowTransition(_, `state`, _) => () // ok

  // temporary, until this is doable via graphql
  def approveConfigurationRequestHack(req: ConfigurationRequest.Id): IO[Unit] =
    import skunk.syntax.all.*
    import lucuma.odb.util.Codecs.configuration_request_id
    session.use: s =>
      s.prepareR(sql"update t_configuration_request set c_status = 'approved' where c_configuration_request_id = $configuration_request_id".command).use: ps =>
        ps.execute(req).void



  //
  // TESTS START HERE
  ///

  import ObservationWorkflowState.*
  
  test(s"Undefined  <-> Inactive"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- assertIO(queryObservationWorkflowState(oid), Undefined)   
      _   <- testTransitions(oid, Undefined, Inactive)
    } yield oid

  test("Unapproved <-> Inactive") {
    for {
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, Some(cfp), None, "Foo")
      _   <- addPartnerSplits(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- computeItcResult(oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Unapproved)   
      _   <- testTransitions(oid, Unapproved, Inactive)
    } yield oid
  }

  test("Defined    <-> Inactive        (proposal not yet accepted)"):
    for {
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, Some(cfp), None, "Foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
      _   <- computeItcResult(oid)
      _   <- assertIO(queryObservationWorkflowState(oid), Defined)   
      _   <- testTransitions(oid, Defined, Inactive)
    } yield oid


  test("Defined    <-> Inactive, Ready (proposal accepted)"):
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
      _   <- assertIO(queryObservationWorkflowState(oid), Defined)   
      _   <- testTransitions(oid, Defined, Inactive, Ready)
    } yield oid

  test("Ongoing    <-> Inactive".ignore):
    ()

  test("Completed  <->".ignore):
    ()

}
