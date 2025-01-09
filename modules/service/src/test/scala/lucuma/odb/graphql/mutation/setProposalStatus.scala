// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramType
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.odb.data.OdbError
import lucuma.odb.data.Tag
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.service.ProposalService.error

class setProposalStatus extends OdbSuite 
  with ObservingModeSetupOperations {

  val pi       = TestUsers.Standard.pi(1, 101)
  val pi2      = TestUsers.Standard.pi(2, 102)
  val ngo      = TestUsers.Standard.ngo(3, 103, Partner.CA)
  val staff    = TestUsers.Standard.staff(4, 104)
  val admin    = TestUsers.Standard.admin(5, 105)
  val guest    = TestUsers.guest(6)

  val validUsers = List(pi, pi2, ngo, staff, admin, guest)

  test("✓ valid submission") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        addPartnerSplits(pi, pid) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "proposal": {
                      "reference": { "label": "G-2025A-0001" }
                    }
                  }
                }
              }
            """.asRight
        )
      }
    }
  }

  test("⨯ missing partner splits (queue)") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected =
            List(s"Submitted proposal $pid of type Queue must specify partner time percentages which sum to 100%.").asLeft
        )
      }
    }
  }

  test("⨯ missing piAffiliation (fast turnaround)") {
    createCallForProposalsAs(staff, CallForProposalsType.FastTurnaround).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some, "fastTurnaround: {}".some) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected =
            List(s"Submitted proposal $pid of type Fast Turnaround must specify the piAffiliation.").asLeft
        )
      }
    }
  }

  test("✓  having piAffiliation (fast turnaround)") {
    createCallForProposalsAs(staff, CallForProposalsType.FastTurnaround).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some, "fastTurnaround: { piAffiliation: US }".some) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program { proposal { reference { label } } }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "proposal": {
                      "reference": { "label": "G-2025A-0002" }
                    }
                  }
                }
              }
            """.asRight

        )
      }
    }
  }

  test("⨯ update proposalStatus with no proposal") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: SUBMITTED
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(error.missingProposal(pid).message))
      )
    }
  }

  test("⨯ pi update proposalStatus to unauthorized status") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: ACCEPTED
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(error.notAuthorizedNew(pid, pi, Tag("accepted")).message))
      )
    }
  }

  test("⨯ guest submit") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      // the non-guest requirement gets caught before it even gets to the service.
      expect(
        user = guest,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(OdbError.NotAuthorized(guest.id).message))
      )
    }
  }

  test("⨯ no CfP for proposal submission") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(error.missingCfP(pid).message))
      )
    }
  }

  test("⨯ non-science program type for proposal submission") {
    createProgramAs(pi).flatMap { pid =>
      setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""") >>
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(error.invalidProgramType(pid, ProgramType.Calibration).message))
      )
    }
  }

  def setCallId(pid: Program.Id, cid: CallForProposals.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateProposal(
            input: {
              programId: "$pid",
              SET: {
                callId: "$cid"
              }
            }
          ) {
            proposal { title }
          }
        }
      """
    ).void

  test("✓ edit proposal status (pi can set to SUBMITTED and back to NOT_SUBMITTED)") {

    def submit(pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: SUBMITTED,
              }
            ) {
              program {
                id
                proposalStatus
                proposal { reference { label } }
              }
            }
          }
        """,
        expected =
          json"""
            {
              "setProposalStatus" : {
                "program": {
                  "id" : $pid,
                  "proposalStatus": "SUBMITTED",
                  "proposal": { "reference": { "label": "G-2025A-0003" } }
                 }
              }
            }
          """.asRight
      )

    def recall(pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: NOT_SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          json"""
            {
              "setProposalStatus" : {
                "program" : {
                  "id" : $pid,
                  "proposalStatus": "NOT_SUBMITTED"
                }
              }
            }
          """.asRight
      )

    def expected(pid: Program.Id): List[Json] =
        List(
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : false,
            "c_new_name"            : null,
            "c_operation"           : "UPDATE",
            "c_mod_pts_pi"          : false,
            "c_new_pts_pi"          : null,
            "c_program_id"          : $pid,
            "c_mod_existence"       : false,
            "c_new_existence"       : null,
            "c_mod_program_id"      : false,
            "c_new_program_id"      : null,
            "c_mod_pts_execution"   : false,
            "c_mod_pts_uncharged"   : false,
            "c_new_pts_execution"   : null,
            "c_new_pts_uncharged"   : null,
            "c_mod_proposal_status" : true,
            "c_new_proposal_status" : "submitted"
          }
          """,
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : false,
            "c_new_name"            : null,
            "c_operation"           : "UPDATE",
            "c_mod_pts_pi"          : false,
            "c_new_pts_pi"          : null,
            "c_program_id"          : $pid,
            "c_mod_existence"       : false,
            "c_new_existence"       : null,
            "c_mod_program_id"      : false,
            "c_new_program_id"      : null,
            "c_mod_pts_execution"   : false,
            "c_mod_pts_uncharged"   : false,
            "c_new_pts_execution"   : null,
            "c_new_pts_uncharged"   : null,
            "c_mod_proposal_status" : true,
            "c_new_proposal_status" : "not_submitted"
          }
          """
        )

    for {
      c <- createCallForProposalsAs(staff, semester = Semester.unsafeFromString("2025A"))
      p <- createProgramAs(pi)
      _ <- addProposal(pi, p)
      _ <- setCallId(p, c)
      _ <- addPartnerSplits(pi, p)
      _ <- submit(p)
      _ <- recall(p)
      l <- chronProgramUpdates(p)
    } yield assertEquals(l.drop(3), expected(p))
  }

  test("⨯ edit proposal status (staff can set to ACCEPTED, and pi cannot change it again)") {

    def accept(pid: Program.Id): IO[Unit] =
      expect(
        user = staff,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: ACCEPTED,
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          json"""
            {
              "setProposalStatus" : {
                "program": {
                  "id" : $pid,
                  "proposalStatus": "ACCEPTED"
                 }
              }
            }
          """.asRight
      )

    def recall(pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                status: NOT_SUBMITTED
                programId: "$pid"
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          List(error.notAuthorizedOld(pid, pi, Tag("accepted")).message).asLeft
      )

    for {
      c <- createCallForProposalsAs(staff, semester = Semester.unsafeFromString("2025A"))
      p <- createProgramAs(pi)
      _ <- addProposal(pi, p)
      _ <- addPartnerSplits(pi, p)
      _ <- setCallId(p, c)
      _ <- accept(p)
      _ <- recall(p)
    } yield ()
  }

  test("⨯ user cannot set status of another user's proposal") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi2,
        query = s"""
          mutation {
            setProposalStatus(
              input: {
                programId: "$pid"
                status: SUBMITTED
              }
            ) {
              program {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(OdbError.InvalidProgram(pid).message))
      )
    }
  }

  test("⨯ attempt to set proposal status in non-existent program") {
    val badPid = Program.Id.fromLong(Long.MaxValue).get
    expect(
      user = pi,
      query = s"""
        mutation {
          setProposalStatus(
            input: {
              programId: "$badPid"
              status: SUBMITTED
            }
          ) {
            program {
              id
              proposalStatus
            }
          }
        }
      """,
      expected =
        Left(List(OdbError.InvalidProgram(badPid).message))
    )
  }

  test("ensure that configuration requests are created when the proposal is submitted, but not for inactive observations or calibrations") {
    for
      cid <- createCallForProposalsAs(staff, CallForProposalsType.RegularSemester)
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      ina <- createObservationAs(pi, pid) // inactive, should be ignored
      _   <- setObservationWorkflowState(pi, ina, ObservationWorkflowState.Inactive)
      cal <- createObservationAs(pi, pid) // calibration, should be ignored
      _   <- setObservationCalibratioRole(cal, Some(CalibrationRole.Photometric))
      _   <-
        expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  configurationRequests {
                    matches {
                      status
                    }
                  }
                }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "configurationRequests" : {
                      "matches" : [
                        {
                          "status" : "REQUESTED"
                        }
                      ]
                    }
                  }
                }
              }
            """.asRight
        )
    yield ()

  }

  test("ensure that configuration requests are deleted when the proposal is withdrawn") {
    for
      cid <- createCallForProposalsAs(staff, CallForProposalsType.RegularSemester)
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <-
        query(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: SUBMITTED
                }
              ) {
                program {
                  id
                }
              }
            }
          """
        )
      _ <- expect(
          user = pi,
          query = s"""
            mutation {
              setProposalStatus(
                input: {
                  programId: "$pid"
                  status: NOT_SUBMITTED
                }
              ) {
                program {
                  configurationRequests {
                    matches {
                      status
                    }
                  }
                }
              }
            }
          """,
          expected =
            json"""
              {
                "setProposalStatus": {
                  "program": {
                    "configurationRequests" : {
                      "matches" : []
                    }
                  }
                }
              }
            """.asRight
        )
    yield ()

  }

}
