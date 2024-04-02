// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import io.circe.literal.*
import lucuma.core.enums.ProgramType
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.odb.data.OdbError
import lucuma.odb.data.Tag
import lucuma.odb.service.ProposalService.UpdateProposalError

class setProposalStatus extends OdbSuite {
  
  val pi       = TestUsers.Standard.pi(1, 101)
  val pi2      = TestUsers.Standard.pi(2, 102)
  val ngo      = TestUsers.Standard.ngo(3, 103, Partner.Ca)
  val staff    = TestUsers.Standard.staff(4, 104)
  val admin    = TestUsers.Standard.admin(5, 105)
  val guest    = TestUsers.guest(6)

  val validUsers = List(pi, pi2, ngo, staff, admin, guest)
  
  test("edit proposal status (attempt update proposalStatus with no proposal)") {
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
          Left(List(UpdateProposalError.NoProposalForStatusChange(pid).message))
      )
    }
  }

  test("edit proposal status (pi attempts update proposalStatus to unauthorized status)") {
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
          Left(List(UpdateProposalError.NotAuthorizedNewProposalStatus(pid, pi, Tag("accepted")).message))
      )
    }
  }

  test("edit proposal status (guests cannot submit proposals)") {
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

  test("no semester for proposal submission") {
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
          Left(List(UpdateProposalError.NoSemesterForSubmittedProposal(pid).message))
      )
    }
  }

  test("non-science program type for proposal submission") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      setProgramReference(pi, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""") >>
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
          Left(List(UpdateProposalError.InvalidProgramType(pid, ProgramType.Calibration).message))
      )
    }
  }

  test("edit proposal status (pi can set to SUBMITTED and back to NOT_SUBMITTED)") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      setSemester(pi, pid, Semester.unsafeFromString("2024B")) >>
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
        expected = Right(
          json"""
            {
              "setProposalStatus" : {
                "program": { 
                  "id" : $pid,
                  "proposalStatus": "SUBMITTED",
                  "proposal": { "reference": { "label": "G-2024B-0001" } }
                 }
              }
            }
          """
        )
      ) >>
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
        expected = Right(
          json"""
            {
              "setProposalStatus" : {
                "program" : { 
                  "id" : $pid,
                  "proposalStatus": "NOT_SUBMITTED"
                }
              }
            }
          """
        )
      ) >>
      chronProgramUpdates(pid).map(_.drop(3)).assertEquals(
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
            "c_mod_pi_user_id"      : false,
            "c_mod_program_id"      : false,
            "c_new_pi_user_id"      : null,
            "c_new_program_id"      : null,
            "c_mod_pi_user_type"    : false,
            "c_new_pi_user_type"    : null,
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
            "c_mod_pi_user_id"      : false,
            "c_mod_program_id"      : false,
            "c_new_pi_user_id"      : null,
            "c_new_program_id"      : null,
            "c_mod_pi_user_type"    : false,
            "c_new_pi_user_type"    : null,
            "c_mod_pts_execution"   : false,
            "c_mod_pts_uncharged"   : false,
            "c_new_pts_execution"   : null,
            "c_new_pts_uncharged"   : null,
            "c_mod_proposal_status" : true,
            "c_new_proposal_status" : "not_submitted"
          }
          """
        )
      )
    }
  }

  test("edit proposal status (staff can set to ACCEPTED, and pi cannot change it again)") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      setSemester(pi, pid, Semester.unsafeFromString("2024B")) >>
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
        expected = Right(
          json"""
            {
              "setProposalStatus" : {
                "program": { 
                  "id" : $pid,
                  "proposalStatus": "ACCEPTED"
                 }
              }
            }
          """
        )
      ) >>
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
          Left(List(UpdateProposalError.NotAuthorizedOldProposalStatus(pid, pi, Tag("accepted")).message))
      )
    }
  }

  test("user cannot set status of another user's proposal") {
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

  test("attempt to set proposal status in non-existent program") {
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

}
