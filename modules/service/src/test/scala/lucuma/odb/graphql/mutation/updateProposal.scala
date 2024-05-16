// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.Program
import lucuma.odb.data.CallForProposalsType

class updateProposal extends OdbSuite {
  
  val pi    = TestUsers.Standard.pi(nextId, nextId)
  val pi2   = TestUsers.Standard.pi(nextId, nextId)
  val guest = TestUsers.guest(nextId)
  val staff = TestUsers.Standard.staff(nextId, nextId)

  val validUsers = List(pi, pi2, guest, staff)

  test("✓ generic properties") {
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

  test("✓ type-specific properties") {
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
                  type: {
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
                type {
                  ... on Queue {
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
                "type": {
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

  test("✓ change type") {
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
                  type: {
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
                type {
                  scienceSubtype
                  ... on DemoScience {
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
                "type": {
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
                  type: {
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
                type {
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
                  type: {
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
          List("Argument 'input.SET.type.queue.partnerSplits' is invalid: Percentages must sum to exactly 100.").asLeft
      )
    }
  }

  test("⨯ set invalid cfp id") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: { callId: "c-123" }
              }
            ) {
              proposal { title }
            }
          }
        """,
        expected =
          List("The specified Call for Proposals (c-123) was not found.").asLeft
      )
    }
  }

  test("⨯ set mismatched cfp") {
    createCallForProposalsAs(staff, CallForProposalsType.DemoScience).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, title = "initial title") *>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateProposal(
                input: {
                  programId: "$pid"
                  SET: { callId: "$cid" }
                }
              ) {
                proposal { title }
              }
            }
          """,
          expected =
            List(s"The Call for Proposals ($cid) is a Demo Science call and cannot be used with a Queue proposal.").asLeft
        )
      }
    }
  }

  test("✓ set matching cfp") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, title = "initial title") *>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateProposal(
                input: {
                  programId: "$pid"
                  SET: { callId: "$cid" }
                }
              ) {
                proposal { type { scienceSubtype } }
              }
            }
          """,
          expected = json"""
            {
              "updateProposal": {
                "proposal": {
                  "type": {
                    "scienceSubtype": "QUEUE"
                  }
                }
              }
            }
          """.asRight
        )
      }
    }
  }

  test("⨯ invalid type change") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, title = "initial title") *>
        query(
          user = pi,
          query = s"""
            mutation {
              updateProposal(
                input: {
                  programId: "$pid"
                  SET: { callId: "$cid" }
                }
              ) {
                proposal { title }
              }
            }
          """
        ) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateProposal(
                input: {
                  programId: "$pid"
                  SET: {
                    type: {
                      demoScience: {
                        toOActivation: STANDARD
                        minPercentTime: 50
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
            List(s"The Call for Proposals ($cid) is a Regular Semester call and cannot be used with a Demo Science proposal.").asLeft
        )
      }
    }
  }

  test("✓ change call and type") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, title = "initial title") *>
        query(
          user = pi,
          query = s"""
            mutation {
              updateProposal(
                input: {
                  programId: "$pid"
                  SET: { callId: "$cid" }
                }
              ) {
                proposal { title }
              }
            }
          """
        ) *>
        createCallForProposalsAs(staff, CallForProposalsType.DemoScience).flatMap { cid2 =>
          expect(
            user = pi,
            query = s"""
              mutation {
                updateProposal(
                  input: {
                    programId: "$pid"
                    SET: {
                      callId: "$cid2"
                      type: {
                        demoScience: {
                          toOActivation: STANDARD
                          minPercentTime: 50
                        }
                      }
                    }
                  }
                ) {
                  proposal { type { scienceSubtype } }
                }
              }
            """,
            expected = json"""
              {
                "updateProposal": {
                  "proposal": {
                    "type": {
                      "scienceSubtype": "DEMO_SCIENCE"
                    }
                  }
                }
              }
            """.asRight
          )
        }
      }
    }
  }

  test("✓ delete cfp") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, title = "initial title") *>
        query(
          user = pi,
          query = s"""
            mutation {
              updateProposal(
                input: {
                  programId: "$pid"
                  SET: { callId: "$cid" }
                }
              ) {
                proposal { title }
              }
            }
          """
        ) *>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateProposal(
                input: {
                  programId: "$pid"
                  SET: {
                    callId: null
                    type: {
                      demoScience: {
                        toOActivation: STANDARD
                        minPercentTime: 50
                      }
                    }
                  }
                }
              ) {
                proposal { type { scienceSubtype } }
              }
            }
          """,
          expected = json"""
            {
              "updateProposal": {
                "proposal": {
                  "type": {
                    "scienceSubtype": "DEMO_SCIENCE"
                  }
                }
              }
            }
          """.asRight
        )
      }
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
*/

  test("⨯ update proposal in another user's program") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
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
              proposal { title }
            }
          }
        """,
        expected =
          List(s"Program $pid does not exist, is not visible, or is ineligible for the requested operation.").asLeft
      )
    }
  }

  test("⨯ guest update proposal") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
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
              proposal { title }
            }
          }
        """,
        expected =
          List(s"User ${guest.id} is not authorized to perform this operation.").asLeft
      )
    }
  }

  test("⨯ update proposal in non-existent program") {
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
            proposal { title }
          }
        }
      """,
      expected =
        List(s"Program $badPid does not exist, is not visible, or is ineligible for the requested operation.").asLeft
    )
  }

  test("⨯ update proposal in non-science program") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "initial title") *>
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
              proposal { title }
            }
          }
        """,
        expected =
          List(s"Program $pid is of type Engineering. Only Science programs can have proposals.").asLeft
      )
    }
  }

}
