// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.odb.data.CallForProposalsType
import lucuma.odb.data.OdbError

class createProposal extends OdbSuite with DatabaseOperations  {
  
  val pi       = TestUsers.Standard.pi(1, 101)
  val pi2      = TestUsers.Standard.pi(2, 102)
  val staff    = TestUsers.Standard.staff(3, 103)
  val guest    = TestUsers.guest(4)

  val validUsers = List(pi, pi2, staff, guest)

  test("✓ default to queue") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Queue Proposal"
                  category: COSMOLOGY
                }
              }
            ) {
              proposal {
                title
                category
                callProperties {
                  scienceSubtype
                  ... on CallPropertiesQueue {
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
                "title" : "My Queue Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
                  "scienceSubtype": "QUEUE",
                  "toOActivation": "NONE",
                  "minPercentTime": 0
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("⨯ already exists") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, title = "existing proposal") *>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Demo Science Proposal"
                  callProperties: {
                    demoScience: {
                      toOActivation: NONE
                      minPercentTime: 0
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
          List(s"Proposal creation failed because program $pid already has a proposal.").asLeft
      )
    }
  }

  test("⨯ multiple call properties") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Demo Science Proposal"
                  callProperties: {
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
              proposal { title }
            }
          }
        """,
        expected =
          List("Argument 'input.SET.callProperties' is invalid: Only one of 'classical', 'demoScience', 'directorsTime', 'fastTurnaround', 'largeProgram', 'poorWeather', 'queue' or 'systemVerfication' may be provided.").asLeft
      )
    }
  }

  test("⨯ unknown call") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Demo Science Proposal"
                  callProperties: {
                    demoScience: {
                      callId: "c-123"
                      toOActivation: NONE
                      minPercentTime: 0
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
          List("The specified Call for Proposals (c-123) was not found.").asLeft
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
                  title: "My Demo Science Proposal"
                  callProperties: {
                    demoScience: {
                      callId: "$cid"
                      toOActivation: NONE
                      minPercentTime: 0
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
          List(s"The indicated Call for Proposals ($cid) is a Poor Weather call and cannot be used with a Demo Science proposal.").asLeft
      )

    for {
      cid <- createCallForProposalsAs(staff, CallForProposalsType.PoorWeather)
      pid <- createProgramAs(pi)
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
                  title: "My Demo Science Proposal"
                  callProperties: {
                    demoScience: {
                      callId: "$cid"
                      toOActivation: NONE
                      minPercentTime: 0
                    }
                  }
                }
              }
            ) {
              proposal {
                title
                callProperties {
                  call { id }
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
                  "title" : "My Demo Science Proposal",
                  "callProperties": {
                    "call": {
                      "id": $cid
                    }
                  }
                }
              }
            }
          """.asRight
      )

    for {
      cid <- createCallForProposalsAs(staff, CallForProposalsType.DemoScience)
      pid <- createProgramAs(pi)
      _   <- go(cid, pid)
    } yield ()
  }

  test("✓ classical") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Classical Proposal"
                  category: COSMOLOGY
                  callProperties: {
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
                title
                category
                callProperties {
                  scienceSubtype
                  ... on CallPropertiesClassical {
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
                "title" : "My Classical Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
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

  test("✓ demo science") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Demo Science Proposal"
                  category: COSMOLOGY
                  callProperties: {
                    demoScience: {
                      toOActivation:  NONE
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
            "createProposal" : {
              "proposal" : {
                "title" : "My Demo Science Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
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

  test("✓ director's time") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Director's Time Proposal"
                  category: COSMOLOGY
                  callProperties: {
                    directorsTime: {
                      toOActivation:  NONE
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
                  ... on CallPropertiesDirectorsTime {
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
                "title" : "My Director's Time Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
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
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Fast Turnaround Proposal"
                  category: COSMOLOGY
                  callProperties: {
                    fastTurnaround: {
                      toOActivation:  NONE
                      minPercentTime: 50
                      piAffiliation: US
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
                  ... on CallPropertiesFastTurnaround {
                    toOActivation
                    minPercentTime
                    piAffiliation
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
                "title" : "My Fast Turnaround Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
                  "scienceSubtype": "FAST_TURNAROUND",
                  "toOActivation": "NONE",
                  "minPercentTime": 50,
                  "piAffiliation": "US"
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ large program") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Large Program Proposal"
                  category: COSMOLOGY
                  callProperties: {
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
                title
                category
                callProperties {
                  scienceSubtype
                  ... on CallPropertiesLargeProgram {
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
                "title" : "My Large Program Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
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

  test("✓ poor weather") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Poor Weather Proposal"
                  category: COSMOLOGY
                  callProperties: {
                    poorWeather: {}
                  }
                }
              }
            ) {
              proposal {
                title
                category
                callProperties {
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
                "title" : "My Poor Weather Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
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
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Queue Proposal"
                  category: COSMOLOGY
                  callProperties: {
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
                title
                category
                callProperties {
                  scienceSubtype
                  ... on CallPropertiesQueue {
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
                "title" : "My Queue Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
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
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My System Verification Proposal"
                  category: COSMOLOGY
                  callProperties: {
                    systemVerification: {
                      toOActivation:  NONE
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
                  ... on CallPropertiesSystemVerification {
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
                "title" : "My System Verification Proposal",
                "category" : "COSMOLOGY",
                "callProperties": {
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
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Queue Proposal"
                  callProperties: {
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
              proposal { title }
            }
          }
        """,
        expected =
          List("Argument 'input.SET.callProperties.queue.partnerSplits' is invalid: Percentages must sum to exactly 100.").asLeft
      )
    }
  }

  test("⨯ partner splits empty") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Queue Proposal"
                  callProperties: {
                    queue: {
                      toOActivation:  NONE
                      minPercentTime: 50
                      partnerSplits: []
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

  test("✓ partner splits missing") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: {
                  title: "My Queue Proposal"
                  callProperties: {
                    queue: {
                      toOActivation:  NONE
                      minPercentTime: 50
                    }
                  }
                }
              }
            ) {
              proposal {
                title
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
            "createProposal" : {
              "proposal" : {
                "title" : "My Queue Proposal",
                "callProperties": {
                  "partnerSplits": []
                }
              }
            }
          }
        """.asRight

      )
    }
  }

  test("⨯ guest create a proposal") {
    createProgramAs(guest).flatMap { pid =>
      expect(
        user = guest,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: { title: "My Guest Proposal" }
              }
            ) {
              proposal { title }
            }
          }
        """,
        expected = List(OdbError.NotAuthorized(guest.id).message).asLeft
      )
    }
  }

  test("⨯ create a proposal in another user's program") {
    createProgramAs(pi2).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: { title: "My Proposal for Someone Else" }
              }
            ) {
              proposal { title }
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
                programId: "$badPid"
                SET: { title: "My Ghost Proposal" }
              }
            ) {
              proposal { title }
            }
          }
        """,
        expected = List(OdbError.InvalidProgram(badPid).message).asLeft
      )
  }

  test("⨯ create a proposal in a non-science program") {
    createProgramAs(pi).flatMap { pid =>
      setProgramReference(pi, pid, """example: { instrument: GMOS_SOUTH }""") >>
      expect(
        user = pi,
        query = s"""
          mutation {
            createProposal(
              input: {
                programId: "$pid"
                SET: { title: "My Guest Proposal" }
              }
            ) {
              proposal { title }
            }
          }
        """,
        expected = List(s"Program p-113 is of type Example. Only Science programs can have proposals.").asLeft
      )
    }
  }

}
