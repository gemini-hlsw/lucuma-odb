// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import io.circe.literal._
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.service.ProposalService.UpdateProposalsError

class updateProposal extends OdbSuite {
  
  val pi       = TestUsers.Standard.pi(1, 101)
  val pi2      = TestUsers.Standard.pi(2, 102)

  val validUsers = List(pi, pi2)

  test("update proposal (non-class properties)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
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
                category
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "createProposal" : {
                "proposal": { 
                  "category": "COSMOLOGY"
                }
              }
            }
          """)
      ) >>
      // Now update it, but not the class
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
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
          Right(json"""
            {
              "updateProposal" : {
                "proposal" : {
                  "title" : "updated title",
                  "proposalClass" : {
                    "__typename" : "Queue",
                    "minPercentTime" : 50
                  },
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
            }
          """)
      )
    }
  }
  
  test("update proposal (proposal class, type A -> type A)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  proposalClass: {
                    queue: {
                      minPercentTime: 40
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
                proposalClass {
                  __typename
                  ... on Queue {
                    minPercentTime
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "createProposal" : {
                "proposal": { 
                  "proposalClass": {
                    "__typename" : "Queue",
                    "minPercentTime": 40
                  }
                }
              }
            }
          """)
      ) >>
      // Now update it with a different type-A proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  proposalClass: {
                    classical: {
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                proposalClass {
                  __typename
                  ... on Classical {
                    minPercentTime
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updateProposal" : {
                "proposal" : {
                  "proposalClass" : {
                    "__typename" : "Classical",
                    "minPercentTime" : 50
                  }
                }
              }
            }
          """)
      )
    }
  }
  
  test("update proposal (proposal class, type A -> type B)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  proposalClass: {
                    queue: {
                      minPercentTime: 40
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
                proposalClass {
                  __typename
                  ... on Queue {
                    minPercentTime
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "createProposal" : {
                "proposal": { 
                  "proposalClass": {
                    "__typename" : "Queue",
                    "minPercentTime": 40
                  }
                }
              }
            }
          """)
      ) >>
      // Now update it with a type-B proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  proposalClass: {
                    intensive: {
                      minPercentTime: 60
                      minPercentTotalTime: 10
                      totalTime: {
                        hours: 10.5
                      }
                    }
                  }
                }
              }
            ) {
              proposal {
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
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updateProposal" : {
                "proposal" : {
                  "proposalClass" : {
                    "__typename" : "Intensive",
                    "minPercentTime" : 60,
                    "minPercentTotalTime" : 10,
                    "totalTime" : {
                      "hours" : 10.500000,
                      "iso" : "PT10H30M"
                    }
                  }
                }
              }
            }
          """)
      )
    }
  }
  
  test("update proposal (proposal class, type A -> type B, incomplete)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  proposalClass: {
                    queue: {
                      minPercentTime: 40
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
                proposalClass {
                  __typename
                  ... on Queue {
                    minPercentTime
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "createProposal" : {
                "proposal": { 
                  "proposalClass": {
                    "__typename" : "Queue",
                    "minPercentTime": 40
                  }
                }
              }
            }
          """)
      ) >>
      // Now update it with an incomplete type-B proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  proposalClass: {
                    intensive: {
                      minPercentTime: 60
                    }
                  }
                }
              }
            ) {
              proposal {
                proposalClass {
                  __typename
                  ... on Intensive {
                    minPercentTime
                  }
                }
              }
            }
          }
        """,
        expected = Left(List(UpdateProposalsError.InconsistentUpdate(pid).message))
      )
    }
  }
  
  test("update fails with no existing proposal") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "updated title"
                }
              }
            ) {
              proposal {
                title
              }
            }
          }
        """,
        expected = Left(List(UpdateProposalsError.UpdateFailed(pid).message))
      )
    }
  }
  
  test("partner splits must sum to 100") {
    createProgramWithProposalAs(pi).flatMap { pid =>
      expect(
        user = pi2,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  partnerSplits: [
                    {
                      partner: AR
                      percent: 70
                    }
                    {
                      partner: KECK
                      percent: 31
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
  
  test("partner splits cannot be empty") {
    createProgramWithProposalAs(pi).flatMap { pid =>
      expect(
        user = pi2,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
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
  
  test("user cannot update proposal in another user's program") {
    createProgramWithProposalAs(pi).flatMap { pid =>
      expect(
        user = pi2,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "updated title"
                }
              }
            ) {
              proposal {
                title
              }
            }
          }
        """,
        expected = Left(List(OdbError.InvalidProgram(pid).message))
      )
    }
  }
  
  test("attempt to update proposal in non-existent program") {
    val badPid = Program.Id.fromLong(Long.MaxValue).get
    expect(
      user = pi,
      query = s"""
        mutation {
          updateProposal(
            input: {
              programId: "$badPid"
              SET: {
                title: "updated title"
              }
            }
          ) {
            proposal {
              title
            }
          }
        }
      """,
      expected = Left(List(OdbError.InvalidProgram(badPid).message))
    )
  }
  
}
