// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import io.circe.literal.*
import lucuma.core.enums.ProgramType
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.service.ProposalService.UpdateProposalError

class createProposal extends OdbSuite {
  
  val pi       = TestUsers.Standard.pi(1, 101)
  val pi2      = TestUsers.Standard.pi(2, 102)

  val validUsers = List(pi, pi2)

  test("successful create type A") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Type A Proposal"
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
            ) {
              proposal {
                title
                proposalClass {
                  __typename
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
        """,
        expected =
          Right(
            json"""
              {
                "createProposal" : {
                  "proposal" : {
                    "title" : "My Type A Proposal",
                    "proposalClass" : {
                      "__typename" : "Queue",
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
              }
          """
          )
        )
    }
  }

  test("successful create type B") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Type B Proposal"
                  proposalClass: {
                    intensive: {
                      minPercentTime: 40
                      minPercentTotalTime: 20
                      totalTime: {
                        hours: 1.23
                      }
                    }
                  }
                  toOActivation: NONE
                  partnerSplits: [
                    {
                      partner: US
                      percent: 70
                    },
                    {
                      partner: CA
                      percent: 30
                    }
                  ]
                }
              }
            ) {
              proposal {
                title
                proposalClass {
                  __typename
                  ... on Intensive {
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
        """,
        expected =
          Right(
            json"""
              {
                "createProposal" : {
                  "proposal" : {
                    "title" : "My Type B Proposal",
                    "proposalClass" : {
                      "__typename" : "Intensive",
                      "minPercentTime" : 40,
                      "minPercentTotalTime" : 20,
                      "totalTime" : {
                        "hours" : 1.230000,
                        "iso" : "PT1H13M48S"
                      }
                    },
                    "category" : null,
                    "toOActivation" : "NONE",
                    "partnerSplits" : [
                      {
                        "partner" : "US",
                        "percent" : 70
                      },
                      {
                        "partner" : "CA",
                        "percent" : 30
                      }
                    ]
                  }
                }
              }
          """
          )
        )
    }
  }

  test("create fails if proposal already exists"){
    createProgramWithProposalAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Proposal"
                  proposalClass: {
                    intensive: {
                      minPercentTime: 40
                      minPercentTotalTime: 20
                      totalTime: {
                        hours: 1.23
                      }
                    }
                  }
                  toOActivation: NONE
                  partnerSplits: [
                    {
                      partner: US
                      percent: 70
                    },
                    {
                      partner: CA
                      percent: 30
                    }
                  ]
                }
              }
            ) {
              proposal {
                title
              }
            }
          }
        """,
        expected =
          Left(List(UpdateProposalError.CreationFailed(pid).message))
        )
    }
  }
  
  test("attempt create with insufficient information") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Proposal"
                }
              }
            ) {
              proposal {
                title
              }
            }
          }
        """,
        expected = 
          Left(List("Argument 'input.SET' is invalid: All of proposalClass, toOActivation, and partnerSplits are required on creation."))
      )
    }
  }

  test("partner splits must sum to 100"){
    createProgramWithProposalAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Proposal"
                  proposalClass: {
                    intensive: {
                      minPercentTime: 40
                      minPercentTotalTime: 20
                      totalTime: {
                        hours: 1.23
                      }
                    }
                  }
                  toOActivation: NONE
                  partnerSplits: [
                    {
                      partner: US
                      percent: 70
                    },
                    {
                      partner: CA
                      percent: 20
                    }
                  ]
                }
              }
            ) {
              proposal {
                title
              }
            }
          }
        """,
        expected =
          Left(List("Argument 'input.SET.partnerSplits' is invalid: Percentages must sum to exactly 100."))
        )
    }
  }

  test("partner splits cannot be empty"){
    createProgramWithProposalAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Proposal"
                  proposalClass: {
                    intensive: {
                      minPercentTime: 40
                      minPercentTotalTime: 20
                      totalTime: {
                        hours: 1.23
                      }
                    }
                  }
                  toOActivation: NONE
                  partnerSplits: []
                }
              }
            ) {
              proposal {
                title
              }
            }
          }
        """,
        expected =
          Left(List("Argument 'input.SET.partnerSplits' is invalid: Percentages must sum to exactly 100."))
        )
    }
  }

  test("user cannot not create proposal in another user's program"){
    createProgramAs(pi2).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Proposal"
                  proposalClass: {
                    intensive: {
                      minPercentTime: 40
                      minPercentTotalTime: 20
                      totalTime: {
                        hours: 1.23
                      }
                    }
                  }
                  toOActivation: NONE
                  partnerSplits: [
                    {
                      partner: US
                      percent: 70
                    },
                    {
                      partner: CA
                      percent: 30
                    }
                  ]
                }
              }
            ) {
              proposal {
                title
              }
            }
          }
        """,
        expected =
          Left(List(OdbError.InvalidProgram(pid).message))
        )
    }
  }

  test("attempt to create proposal in non-existent program"){
    val badPid = Program.Id.fromLong(Long.MaxValue).get
    expect(
      user = pi,
      query = s"""
        mutation {
          createProposal(
            input: {
              programId: "$badPid"
              SET: {
                title: "My Proposal"
                proposalClass: {
                  intensive: {
                    minPercentTime: 40
                    minPercentTotalTime: 20
                    totalTime: {
                      hours: 1.23
                    }
                  }
                }
                toOActivation: NONE
                partnerSplits: [
                  {
                    partner: US
                    percent: 70
                  },
                  {
                    partner: CA
                    percent: 30
                  }
                ]
              }
            }
          ) {
            proposal {
              title
            }
          }
        }
      """,
      expected =
        Left(List(OdbError.InvalidProgram(badPid).message))
      )
  }

  test("Attempt to create proposal in non-science program"){
    createProgramAs(pi).flatMap { pid =>
      setProgramReference(pi, pid, """example: { instrument: GMOS_SOUTH }""") >>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Proposal"
                  proposalClass: {
                    intensive: {
                      minPercentTime: 40
                      minPercentTotalTime: 20
                      totalTime: {
                        hours: 1.23
                      }
                    }
                  }
                  toOActivation: NONE
                  partnerSplits: [
                    {
                      partner: US
                      percent: 70
                    },
                    {
                      partner: CA
                      percent: 30
                    }
                  ]
                }
              }
            ) {
              proposal {
                title
              }
            }
          }
        """,
        expected =
          Left(List(UpdateProposalError.InvalidProgramType(pid, ProgramType.Example).message))
        )
    }
  }
}
