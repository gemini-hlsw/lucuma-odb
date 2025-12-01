// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.odb.data.OdbError

class createProposal extends OdbSuite with DatabaseOperations {

  val pi       = TestUsers.Standard.pi(1, 101)
  val pi2      = TestUsers.Standard.pi(2, 102)
  val staff    = TestUsers.Standard.staff(3, 103)
  val guest    = TestUsers.guest(4)

  val validUsers = List(pi, pi2, staff, guest)

  test("✓ default to queue") {
    createProgramAs(pi, "My Queue Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on Queue {
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
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "QUEUE",
                  "toOActivation": "NONE",
                  "minPercentTime": 100
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("⨯ already exists") {
    createProgramAs(pi, "My Demo Science Proposal").flatMap { pid =>
      addProposal(pi, pid) *>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  type: {
                    demoScience: {
                      toOActivation: NONE
                      minPercentTime: 0
                    }
                  }
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected =
          List(s"Proposal creation failed because program $pid already has a proposal.").asLeft
      )
    }
  }

  test("⨯ multiple call properties") {
    createProgramAs(pi, "My Demo Science Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  type: {
                    demoScience: {
                      toOActivation: NONE
                      minPercentTime: 0
                    }
                    directorsTime: {
                      toOActivation: NONE
                      minPercentTime: 0
                    }
                  }
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected =
          List("Exactly one key must be specified for oneOf input object ProposalTypeInput in field 'createProposal' of type 'Mutation', but found 'demoScience', 'directorsTime'").asLeft
      )
    }
  }

  test("⨯ unknown call") {
    createProgramAs(pi, "My Demo Science Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  callId: "c-123"
                  type: {
                    demoScience: {
                      toOActivation: NONE
                      minPercentTime: 0
                    }
                  }
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected =
          List("The specified Call for Proposals c-123 was not found.").asLeft
      )
    }
  }

  test("⨯ mismatched call") {
    def go(cid: CallForProposals.Id, pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  callId: "$cid"
                  type: {
                    demoScience: {
                      toOActivation: NONE
                      minPercentTime: 0
                    }
                  }
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected =
          List(s"The Call for Proposals $cid is a Poor Weather call and cannot be used with a Demo Science proposal.").asLeft
      )

    for {
      cid <- createCallForProposalsAs(staff, CallForProposalsType.PoorWeather)
      pid <- createProgramAs(pi, "My Demo Science Proposal")
      _   <- go(cid, pid)
    } yield ()
  }

  test("✓ matched call") {
    def go(cid: CallForProposals.Id, pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  callId: "$cid"
                  type: {
                    demoScience: {
                      toOActivation: NONE
                      minPercentTime: 0
                    }
                  }
                }
              }
            ) {
              proposal {
                call { id }
                type {
                  scienceSubtype
                }
              }
            }
          }
        """,
        expected =
          json"""
            {
              "createProposal" : {
                "proposal" : {
                  "call": {
                    "id": $cid
                  },
                  "type": {
                    "scienceSubtype": "DEMO_SCIENCE"
                  }
                }
              }
            }
          """.asRight
      )

    for {
      cid <- createCallForProposalsAs(staff, CallForProposalsType.DemoScience)
      pid <- createProgramAs(pi,  "My Demo Science Proposal")
      _   <- go(cid, pid)
    } yield ()
  }

  test("✓ assign proprietary period") { // proprietary months match the call
    val months = for {
      cid <- createCallForProposalsAs(staff, CallForProposalsType.DemoScience)
      pid <- createProgramAs(pi)
      _   <- addDemoScienceProposal(pi, pid, cid)
      m   <- getProprietaryMonths(pi, pid)
    } yield m
    assertIO(months, NonNegInt.unsafeFrom(3).some)
  }

  test("✓ don't assign proprietary period") { // no call, no assignment
    val months = for {
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, none,
               """
                 demoScience: {
                   toOActivation: NONE
                   minPercentTime: 0
                 }
               """.some
             )
      m   <- getProprietaryMonths(pi, pid)
    } yield m
    assertIO(months, NonNegInt.unsafeFrom(0).some)
  }

  test("✓ classical") {
    createProgramAs(pi, "My Classical Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    classical: {
                      minPercentTime: 50
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
                category
                type {
                  scienceSubtype
                  ... on Classical {
                    minPercentTime
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
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "CLASSICAL",
                  "minPercentTime": 50,
                  "partnerSplits": [
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
          }
        """.asRight
      )
    }
  }

  test("✓ classical defaults") {
    createProgramAs(pi, "My Classical Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: { classical: { } }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on Classical {
                    minPercentTime
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "CLASSICAL",
                  "minPercentTime": 100
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ demo science") {
    createProgramAs(pi, "My Demo Science Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    demoScience: {
                      toOActivation:  NONE
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
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
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "DEMO_SCIENCE",
                  "toOActivation": "NONE",
                  "minPercentTime": 50
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ demo science defaults") {
    createProgramAs(pi, "My Demo Science Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    demoScience: { }
                  }
                }
              }
            ) {
              proposal {
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
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "DEMO_SCIENCE",
                  "toOActivation": "NONE",
                  "minPercentTime": 100
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ director's time") {
    createProgramAs(pi, "My Director's Time Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    directorsTime: {
                      toOActivation:  NONE
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on DirectorsTime {
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
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "DIRECTORS_TIME",
                  "toOActivation": "NONE",
                  "minPercentTime": 50
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ fast turnaround") {
    createProgramAs(pi, "My Fast Turnaround Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    fastTurnaround: {
                      toOActivation:  NONE
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on FastTurnaround {
                    toOActivation
                    minPercentTime
                    reviewer {
                      id
                      role
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "FAST_TURNAROUND",
                  "toOActivation": "NONE",
                  "minPercentTime": 50,
                  "reviewer": null
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ fast turnaround defaults") {
    createProgramAs(pi, "My Fast Turnaround Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    fastTurnaround: { }
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on FastTurnaround {
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
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "FAST_TURNAROUND",
                  "toOActivation": "NONE",
                  "minPercentTime": 100
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ fast turnaround with reviewer") {
    for {
      pid  <- createProgramAs(pi, "Fast Turnaround Proposal")
      // Add a COI who will be the reviewer
      puId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _    <- expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    fastTurnaround: {
                      toOActivation: NONE
                      minPercentTime: 50
                      reviewerId: "$puId"
                    }
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on FastTurnaround {
                    toOActivation
                    minPercentTime
                    reviewer {
                      id
                      role
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal": {
              "proposal": {
                "category": "COSMOLOGY",
                "type": {
                  "scienceSubtype": "FAST_TURNAROUND",
                  "toOActivation": "NONE",
                  "minPercentTime": 50,
                  "reviewer": {
                    "id": ${puId.toString},
                    "role": "COI"
                  }
                }
              }
            }
          }
        """.asRight
      )
    } yield ()
  }

  test("⨯ fast turnaround with non-existent reviewer") {
    createProgramAs(pi, "Fast Turnaround Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    fastTurnaround: {
                      toOActivation: NONE
                      minPercentTime: 50
                      reviewerId: "pu-ffff-ffff-ffff-ffff"
                    }
                  }
                }
              }
            ) {
              proposal {
                category
              }
            }
          }
        """,
        expected = List("Argument 'input.SET.type.fastTurnaround.reviewerId' is invalid: 'pu-ffff-ffff-ffff-ffff' is not a valid program user id").asLeft
      )
    }
  }

  test("✓ large program") {
    createProgramAs(pi, "My Large Program Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    largeProgram: {
                      toOActivation:  NONE
                      minPercentTime: 50
                      minPercentTotalTime: 75
                      totalTime: { hours: 500.0 }
                    }
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on LargeProgram {
                    toOActivation
                    minPercentTime
                    minPercentTotalTime
                    totalTime { hours }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "LARGE_PROGRAM",
                  "toOActivation": "NONE",
                  "minPercentTime": 50,
                  "minPercentTotalTime": 75,
                  "totalTime": {
                    "hours": 500.000000
                  }
                }
              }
            }
          }
        """.asRight
      )
    }
  }


  test("✓ large program defaults") {
    createProgramAs(pi, "My Large Program Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    largeProgram: { }
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on LargeProgram {
                    toOActivation
                    minPercentTime
                    minPercentTotalTime
                    totalTime { hours }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "LARGE_PROGRAM",
                  "toOActivation": "NONE",
                  "minPercentTime": 100,
                  "minPercentTotalTime": 100,
                  "totalTime": {
                    "hours": 0.000000
                  }
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ poor weather") {
    createProgramAs(pi, "My Poor Weather Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    poorWeather: {}
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "POOR_WEATHER"
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ poor weather explicit dummy") {
    createProgramAs(pi, "My Poor Weather Proposal 2").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    poorWeather: {
                      ignore: IGNORE
                    }
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "POOR_WEATHER"
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ queue") {
    createProgramAs(pi, "My Queue Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    queue: {
                      toOActivation:  NONE
                      minPercentTime: 50
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
                category
                type {
                  scienceSubtype
                  ... on Queue {
                    toOActivation
                    minPercentTime
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
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "QUEUE",
                  "toOActivation": "NONE",
                  "minPercentTime": 50,
                  "partnerSplits": [
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
          }
        """.asRight
      )
    }
  }

  test("✓ system verification") {
    createProgramAs(pi, "My System Verification Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    systemVerification: {
                      toOActivation:  NONE
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                category
                type {
                  scienceSubtype
                  ... on SystemVerification {
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
            "createProposal" : {
              "proposal" : {
                "category" : "COSMOLOGY",
                "type": {
                  "scienceSubtype": "SYSTEM_VERIFICATION",
                  "toOActivation": "NONE",
                  "minPercentTime": 50
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("⨯ partner splits sum to 100") {
    createProgramAs(pi, "My Queue Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  type: {
                    queue: {
                      toOActivation:  NONE
                      minPercentTime: 50
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
              proposal { category }
            }
          }
        """,
        expected =
          List("Argument 'input.SET.type.queue.partnerSplits' is invalid: Percentages must sum to exactly 100.").asLeft
      )
    }
  }

  test("⨯ partner splits empty") {
    createProgramAs(pi, "My Queue Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  type: {
                    queue: {
                      toOActivation:  NONE
                      minPercentTime: 50
                      partnerSplits: []
                    }
                  }
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected =
          List("Argument 'input.SET.type.queue.partnerSplits' is invalid: Percentages must sum to exactly 100.").asLeft
      )
    }
  }

  test("✓ partner splits missing") {
    createProgramAs(pi, "My Queue Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  type: {
                    queue: {
                      toOActivation:  NONE
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                type {
                  ... on Queue {
                    scienceSubtype
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal" : {
              "proposal" : {
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

  test("⨯ guest create a proposal") {
    createProgramAs(guest, "My Guest Proposal").flatMap { pid =>
      expect(
        user = guest,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid",
                SET: {
                  category: GALACTIC_OTHER
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected = List(OdbError.NotAuthorized(guest.id).message).asLeft
      )
    }
  }

  test("⨯ create a proposal in another user's program") {
    createProgramAs(pi2, "My Proposal for Someone Else").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid",
                SET: {
                  category: GALACTIC_OTHER
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected = List(OdbError.InvalidProgram(pid).message).asLeft
      )
    }
  }

  test("⨯ create a proposal in a non-existent program") {
    val badPid = Program.Id.fromLong(Long.MaxValue).get
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$badPid",
                SET: {
                  category: GALACTIC_OTHER
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected = List(OdbError.InvalidProgram(badPid).message).asLeft
      )
  }

  test("⨯ create a proposal in a non-science program") {
    createProgramAs(pi, "My Guest Proposal" ).flatMap { pid =>
      setProgramReference(staff, pid, """example: { instrument: GMOS_SOUTH }""") >>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid",
                SET: {
                  category: GALACTIC_OTHER
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected = List(s"Program $pid is of type Example. Only Science programs can have proposals.").asLeft
      )
    }
  }

  test("✓ fast turnaround returns null reviewer and mentor by default") {
    createProgramAs(pi, "Fast Turnaround Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    fastTurnaround: {
                      toOActivation: NONE
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                type {
                  ... on FastTurnaround {
                    scienceSubtype
                    reviewer {
                      role
                    }
                    mentor {
                      id
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "createProposal": {
              "proposal": {
                "type": {
                  "scienceSubtype": "FAST_TURNAROUND",
                  "reviewer": null,
                  "mentor": null
                }
              }
            }
          }
        """.asRight
      )
    }
  }


  test("✓ fast turnaround with reviewer and mentor") {
    for {
      pid        <- createProgramAs(pi, "Fast Turnaround Proposal")
      reviewerId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      mentorId   <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _          <- createFastTurnaroundProposal(pi, pid, Some(reviewerId.toString), Some(mentorId.toString))
    } yield ()
  }

  test("⨯ fast turnaround with same user as reviewer and mentor") {
    for {
      pid  <- createProgramAs(pi, "Fast Turnaround Proposal")
      puId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _    <- createFastTurnaroundProposalError(pi, pid, puId.toString)
    } yield ()
  }

  test("✓ fast turnaround with only mentor") {
    for {
      pid      <- createProgramAs(pi, "Fast Turnaround Proposal")
      mentorId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _        <- createFastTurnaroundProposal(pi, pid, None, Some(mentorId.toString))
    } yield ()
  }

  test("⨯ queue proposal cannot have reviewerId") {
    createProgramAs(pi, "Queue Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    queue: {
                      reviewerId: "pu-1234-5678-9abc-def0"
                    }
                  }
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected = List("Unknown field(s) 'reviewerId' for input object value of type QueueInput in field 'createProposal' of type 'Mutation'").asLeft
      )
    }
  }

  test("⨯ queue proposal cannot have mentorId") {
    createProgramAs(pi, "Queue Proposal").flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                  type: {
                    queue: {
                      mentorId: "pu-1234-5678-9abc-def0"
                    }
                  }
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected = List("Unknown field(s) 'mentorId' for input object value of type QueueInput in field 'createProposal' of type 'Mutation'").asLeft
      )
    }
  }

}
