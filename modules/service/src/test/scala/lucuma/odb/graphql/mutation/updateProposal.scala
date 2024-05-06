// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.syntax.either.*
import io.circe.literal.*
//import lucuma.core.enums.ProgramType
//import lucuma.core.model.Program
//import lucuma.odb.data.OdbError
//import lucuma.odb.service.ProposalService.UpdateProposalError

class updateProposal extends OdbSuite {
  
  val pi       = TestUsers.Standard.pi(1, 101)
  val pi2      = TestUsers.Standard.pi(2, 102)
  val guest    = TestUsers.guest(3)

  val validUsers = List(pi, pi2, guest)

  test("✓ non-call properties") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
      // Now update it, but not the call type
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
                }
              }
            ) {
              proposal {
                title
                category
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProposal": {
              "proposal": {
                "title": "updated title",
                "category": "SMALL_BODIES"
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ call properties") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  callProperties: {
                    queue: {
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
                }
              }
            ) {
              proposal {
                callProperties {
                  ... on CallPropertiesQueue {
                    partnerSplits {
                      partner
                      percent
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProposal": {
              "proposal": {
                "callProperties": {
                  "partnerSplits": [
                    {
                      "partner": "US",
                      "percent": 70
                    },
                    {
                      "partner": "CA",
                      "percent": 30
                    }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ change call") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
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
                  callProperties: {
                    demoScience: {
                      toOActivation: STANDARD
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                title
                category
                callProperties {
                  scienceSubtype
                  ... on CallPropertiesDemoScience {
                    toOActivation
                    minPercentTime
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProposal": {
              "proposal": {
                "title": "updated title",
                "category": "SMALL_BODIES",
                "callProperties": {
                  "scienceSubtype": "DEMO_SCIENCE",
                  "toOActivation": "STANDARD",
                  "minPercentTime": 50
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("⨯ missing properties") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
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
                  callProperties: {
                    fastTurnaround: {
                      toOActivation: STANDARD
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                title
                callProperties {
                  scienceSubtype
                }
              }
            }
          }
        """,
        expected =
          List("'toOActivation', 'minPercentTime' and 'piAffiliate' are required for fast turnaround proposals.").asLeft
      )
    }
  }

  test("⨯ missing proposal") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: { title: "updated title" }
              }
            ) {
              proposal { title }
            }
          }
        """,
        expected =
          List(s"Proposal update failed because program $pid does not have a proposal.").asLeft
      )
    }
  }

  test("⨯ splits sum to 100") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  callProperties: {
                    queue: {
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
                }
              }
            ) {
              proposal { title }
            }
          }
        """,
        expected =
          List("Argument 'input.SET.callProperties.queue.partnerSplits' is invalid: Percentages must sum to exactly 100.").asLeft
      )
    }
  }

/*
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
  
  test("guest cannot update proposal") {
    createProgramWithProposalAs(pi).flatMap { pid =>
      // the non-guest requirement gets caught before it even gets to the service.
      expect(
        user = guest,
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
        expected = Left(List(OdbError.NotAuthorized(guest.id).message))
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
  
  test("Attempt to update proposal in non-science program") {
    createProgramWithProposalAs(pi).flatMap { pid =>
      setProgramReference(pi, pid, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""") >>
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
        expected = Left(List(UpdateProposalError.InvalidProgramType(pid, ProgramType.Engineering).message))
      )
    }
  }
 */
}
