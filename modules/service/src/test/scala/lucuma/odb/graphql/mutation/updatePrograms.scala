// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.Tag
import lucuma.odb.service.ProgramService.UpdateProgramsError
import lucuma.odb.service.ProposalService.UpdateProposalsError

class updatePrograms extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.Ca)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service).toList

  test("edit name") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  name: "new name"
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              hasMore
              programs {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": {
              "hasMore": false,
              "programs": [
                {
                  "id": $pid,
                  "name": "new name"
                }
              ]
            }
          }
          """
        )
      ) >> chronProgramUpdates(pid).map(_.drop(1)).assertEquals(
        List(
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : true,
            "c_new_name"            : "new name",
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
            "c_mod_proposal_status" : false,
            "c_new_proposal_status" : null
          }
          """
        )
      )
    }
  }

  test("edit existence") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  existence: DELETED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
                includeDeleted: true
              }
            ) {
              programs {
                id
                existence
              }
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": {
              "programs": [
                {
                  "id": $pid,
                  "existence": ${Existence.Deleted:Existence}
                }
              ]
            }
          }
          """
        )
      ) >> chronProgramUpdates(pid).map(_.drop(1)).assertEquals(
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
            "c_mod_existence"       : true,
            "c_new_existence"       : "deleted",
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
            "c_mod_proposal_status" : false,
            "c_new_proposal_status" : null
          }
          """
        )
      )
    }
  }


  test("edit proposal (attempt add w/ insufficient information)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "new title"
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  title
                }
              }
            }
          }
        """,
        expected =
          Left(List(UpdateProposalsError.CreationFailed(pi).message))
      )
    }
  }

  test("edit proposal (add type A)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "new title"
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    category: COSMOLOGY
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  title
                  proposalClass {
                    ... on Queue {
                      minPercentTime
                    }
                  }
                  category
                  toOActivation
                  partnerSplits {
                    partner
                    percent
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposal" : {
                      "title" : "new title",
                      "proposalClass" : {
                        "minPercentTime" : 50
                      },
                      "category" : "COSMOLOGY",
                      "toOActivation" : "NONE",
                      "partnerSplits" : [
                        {
                          "partner" : "US",
                          "percent" : 100
                        }
                      ]
                    }
                  }
                ]
              }
            }
          """)
      )
    }
  }

  test("edit proposal (add type B)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "new title"
                    proposalClass: {
                      largeProgram: {
                        minPercentTime: 50
                        minPercentTotalTime: 60
                        totalTime: {
                          hours: 0.5
                        }
                      }
                    }
                    category: GASEOUS_ASTROPHYSICS
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: CL
                        percent: 10
                      },
                      {
                        partner: UH
                        percent: 80
                      },
                      {
                        partner: BR
                        percent: 10
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  title
                  proposalClass {
                    ... on LargeProgram {
                      minPercentTime
                      minPercentTotalTime
                      totalTime {
                        hours
                        iso
                      }
                    }
                  }
                  category
                  toOActivation
                  partnerSplits {
                    partner
                    percent
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposal" : {
                      "title" : "new title",
                      "proposalClass" : {
                        "minPercentTime" : 50,
                        "minPercentTotalTime" : 60,
                        "totalTime" : {
                          "hours" : 0.500000,
                          "iso" : "PT30M"
                        }
                      },
                      "category" : "GASEOUS_ASTROPHYSICS",
                      "toOActivation" : "NONE",
                      "partnerSplits" : [
                        {
                          "partner" : "UH",
                          "percent" : 80
                        },
                        {
                          "partner" : "BR",
                          "percent" : 10
                        },
                        {
                          "partner" : "CL",
                          "percent" : 10
                        }
                      ]
                    }
                  }
                ]
              }
            }
          """)
      )
    }
  }

  test("edit proposal (non-class properties)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    category: COSMOLOGY
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid
                  }
                ]
              }
            }
          """)
      ) >>
      // Now update it with a different type-A proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "updated title"
                    category: SMALL_BODIES
                    toOActivation: RAPID
                    partnerSplits: [
                      {
                        partner: AR
                        percent: 70
                      }
                      {
                        partner: KECK
                        percent: 30
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  title
                  category
                  toOActivation
                  partnerSplits {
                    partner
                    percent
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposal" : {
                      "title" : "updated title",
                      "category" : "SMALL_BODIES",
                      "toOActivation" : "RAPID",
                      "partnerSplits" : [
                        {
                          "partner" : "AR",
                          "percent" : 70
                        },
                        {
                          "partner" : "KECK",
                          "percent" : 30
                        }
                      ]
                    }
                  }
                ]
              }
            }
          """)
      )

    }
  }

  test("edit proposal (proposal class, type A -> type A)") {
    createProgramAs(pi).flatMap { pid =>

      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid
                  }
                ]
              }
            }
          """)
      ) >>
      // Now update it with a different type-A proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      classical: {
                        minPercentTime: 40
                      }
                    }
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  proposalClass {
                    ... on Classical {
                      minPercentTime
                    }
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposal" : {
                      "proposalClass" : {
                        "minPercentTime" : 40
                      }
                    }
                  }
                ]
              }
            }
          """)
      )

    }

  }

  test("edit proposal (proposal class, type A -> type B, incomplete)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid
                  }
                ]
              }
            }
          """)
      ) >>
      // Now update it with an incomplete type-B proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      intensive: {
                        minPercentTime: 40
                      }
                    }
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  proposalClass {
                    ... on Intensive {
                      minPercentTime
                    }
                  }
                }
              }
            }
          }
        """,
        expected = Left(List(UpdateProposalsError.InconsistentUpdate(pi).message))
      )

    }

  }

  test("edit proposal (proposal class, type A -> type B)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    category: EXOPLANET_ATMOSPHERES_ACTIVITY
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid
                  }
                ]
              }
            }
          """)
      ) >>
      // Now update it with an valid type-B proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      intensive: {
                        minPercentTime: 40
                        minPercentTotalTime: 10
                        totalTime: {
                          hours: 10.5
                        }
                      }
                    }
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  category
                  proposalClass {
                    ... on Intensive {
                      minPercentTime
                      minPercentTotalTime
                      totalTime {
                        hours
                        iso
                      }
                    }
                  }
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposal" : {
                      "category" : "EXOPLANET_ATMOSPHERES_ACTIVITY",
                      "proposalClass" : {
                        "minPercentTime" : 40,
                        "minPercentTotalTime" : 10,
                        "totalTime" : {
                          "hours" : 10.500000,
                          "iso" : "PT10H30M"
                        }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )

    }

  }

  test("bulk update basic properties") {
    // create a bunch and edit a few of them
    createProgramAs(pi).replicateA(10).flatMap { pids =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  name: "updated"
                }
                WHERE: {
                  id: {
                    IN: [ ${pids.take(3).mkString("\"", "\", \"", "\"")} ]
                  }
                }
              }
            ) {
              programs {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : ${pids(0)},
                    "name" : "updated"
                  },
                  {
                    "id" : ${pids(1)},
                    "name" : "updated"
                  },
                  {
                    "id" : ${pids(2)},
                    "name" : "updated"
                  }
                ]
              }
            }
          """
        )
      )

    }
  }

  test("bulk update proposal: one insert, one update") {
    (createProgramAs(pi), createProgramAs(pi)).tupled.flatMap { (pid1, pid2) =>
      // Add a proposal to one of them
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "my proposal"
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid1"
                  }
                }
              }
            ) {
              programs {
                id
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid1
                  }
                ]
              }
            }
          """
        )
      ) >>
      // Now update both of them
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      classical: {
                        minPercentTime: 30
                      }
                    }
                    toOActivation: RAPID
                    partnerSplits: [
                      {
                        partner: KECK
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    IN: ["$pid1", "$pid2"]
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  title
                  proposalClass {
                    ... on Classical {
                      minPercentTime
                    }
                  }
                  toOActivation
                  partnerSplits {
                    partner
                    percent
                  }
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid1,
                    "proposal" : {
                      "title": "my proposal",
                      "proposalClass" : {
                        "minPercentTime" : 30
                      },
                      "toOActivation": "RAPID",
                      "partnerSplits" : [
                        {
                          "partner" : "KECK",
                          "percent" : 100
                        }
                      ]
                    }
                  },
                  {
                    "id" : $pid2,
                    "proposal" : {
                      "title": null,
                      "proposalClass" : {
                        "minPercentTime" : 30
                      },
                      "toOActivation": "RAPID",
                      "partnerSplits" : [
                        {
                          "partner" : "KECK",
                          "percent" : 100
                        }
                      ]
                    }
                  }
                ]
              }
            }
          """
        )
      )
    }
  }

  test("edit proposal status (attempt update proposalStatus with no proposal)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: SUBMITTED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(UpdateProgramsError.NoProposalForStatusChange(pid).message))
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
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: ACCEPTED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(UpdateProgramsError.NotAuthorizedNewProposalStatus(pi, Tag("accepted")).message))
      )
    }
  }

  test("edit proposal status (guests cannot submit proposals)") {
    createProgramAs(guest).flatMap { pid =>
      addProposal(guest, pid) >>
      expect(
        user = guest,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: SUBMITTED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(UpdateProgramsError.NotAuthorizedNewProposalStatus(guest, Tag("submitted")).message))
      )
    }
  }

  test("edit proposal status (pi can set to SUBMITTED and back to NOT_SUBMITTED)") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) >>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: SUBMITTED,
                  semester: "2024B"
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
                semester
                semesterIndex
                reference
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposalStatus": "SUBMITTED",
                    "semester": "2024B",
                    "semesterIndex": 1,
                    "reference": "G-2024B-0001"
                  }
                ]
              }
            }
          """
        )
      ) >>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: NOT_SUBMITTED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposalStatus": "NOT_SUBMITTED"
                  }
                ]
              }
            }
          """
        )
      ) >>
      chronProgramUpdates(pid).map(_.drop(1)).assertEquals(
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
      expect(
        user = staff,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: ACCEPTED,
                  semester: "2024B"
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposalStatus": "ACCEPTED"
                  }
                ]
              }
            }
          """
        )
      ) >>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: NOT_SUBMITTED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(List(UpdateProgramsError.NotAuthorizedOldProposalStatus(pid, pi, Tag("accepted")).message))
      )
    }
  }

  test("edit proposal status (multiple errors)") {
    (createProgramAs(pi), createProgramAs(pi), createProgramAs(pi)).tupled.flatMap { (pid1, pid2, pid3) =>
      addProposal(pi, pid1) >>
      addProposal(pi, pid2) >>
      // have admin set one to NOT_ACCEPTED
      expect(
        user = admin,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: NOT_ACCEPTED,
                  semester: "2024B"
                }
                WHERE: {
                  id: {
                    EQ: "$pid1"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid1,
                    "proposalStatus": "NOT_ACCEPTED"
                  }
                ]
              }
            }
          """
        )
      ) >>
      // now try to change them all
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: SUBMITTED
                }
                WHERE: {
                  id: {
                    IN: ["$pid1", "$pid2", "$pid3"]
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected =
          Left(
            List(
              UpdateProgramsError.NotAuthorizedOldProposalStatus(pid1, pi, Tag("not_accepted")).message,
              UpdateProgramsError.NoSemesterForSubmittedProposal(pid2).message,
              UpdateProgramsError.NoProposalForStatusChange(pid3).message
            )
          )
      )
    }
  }

  test("edit proposal status (bulk update by current status)") {
    (createProgramAs(pi), createProgramAs(pi), createProgramAs(pi)).tupled.flatMap { (pid1, pid2, pid3) =>
      addProposal(pi, pid1) >>
      addProposal(pi, pid2) >>
      addProposal(pi, pid3) >>
      // have admin set one to ACCEPTED
      expect(
        user = admin,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: ACCEPTED,
                  semester: "2024B"
                }
                WHERE: {
                  id: {
                    EQ: "$pid2"
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid2,
                    "proposalStatus": "ACCEPTED"
                  }
                ]
              }
            }
          """
        )
      ) >>
      // now set all the NOT_SUBMITTED to NOT_ACCEPTED
      // (have to also limit program ids because there are programs from other tests)
      expect(
        user = admin,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: NOT_ACCEPTED,
                  semester: "2024B"
                }
                WHERE: {
                  proposalStatus: {
                    EQ: NOT_SUBMITTED
                  }
                  id: {
                    IN: ["$pid1", "$pid2", "$pid3"]
                  }
                }
              }
            ) {
              programs {
                id
                proposalStatus
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid1,
                    "proposalStatus": "NOT_ACCEPTED"
                  },
                  {
                    "id" : $pid3,
                    "proposalStatus": "NOT_ACCEPTED"
                  }
                ]
              }
            }
          """
        )
      )
    }
  }

}
