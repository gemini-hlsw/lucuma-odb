// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.literal.*
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.util.DateInterval
import lucuma.odb.data.OdbError

import java.time.LocalDate
import java.time.Month

class updateProposal extends OdbSuite with DatabaseOperations {

  val pi    = TestUsers.Standard.pi(nextId, nextId)
  val pi2   = TestUsers.Standard.pi(nextId, nextId)
  val guest = TestUsers.guest(nextId)
  val staff = TestUsers.Standard.staff(nextId, nextId)

  val validUsers = List(pi, pi2, guest, staff)

  test("✓ generic properties") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) *>
      // Now update it, but not the call type
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: SMALL_BODIES
                }
              }
            ) {
              proposal {
                category
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProposal": {
              "proposal": {
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
      addProposal(pi, pid) *>
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
      addProposal(pi, pid) *>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
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
      ) *>
      assertIO(getProprietaryMonths(pi, pid), NonNegInt.unsafeFrom(0).some)
    }
  }

  test("✓ change type to LP") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) *>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: SMALL_BODIES
                  type: {
                    largeProgram: {
                      minPercentTotalTime: 25
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
            "updateProposal": {
              "proposal": {
                "category": "SMALL_BODIES",
                "type": {
                  "scienceSubtype": "LARGE_PROGRAM",
                  "minPercentTotalTime": 25,
                  "totalTime": { "hours": 0.000000 }
                }
              }
            }
          }
        """.asRight
      ) *>
      assertIO(getProprietaryMonths(pi, pid), NonNegInt.unsafeFrom(0).some)
    }
  }

  test("✓ change type from LP") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, callProps =
        s"""
          largeProgram: {
             minPercentTotalTime: 25
             totalTime: { hours: 10.0 }
          }
        """.some
      ) *>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: SMALL_BODIES
                  type: {
                    demoScience: { }
                  }
                }
              }
            ) {
              proposal {
                type {
                  scienceSubtype
                  ... on DemoScience {
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
                "type": {
                  "scienceSubtype": "DEMO_SCIENCE",
                  "minPercentTime": 100
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ change type to PW") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, callProps =
        s"""
          largeProgram: {
             minPercentTime: 50
             toOActivation: STANDARD
             minPercentTotalTime: 25
             totalTime: { hours: 10.0 }
          }
        """.some
      ) *>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: SMALL_BODIES
                  type: {
                    poorWeather: { }
                  }
                }
              }
            ) {
              proposal {
                type {
                  scienceSubtype
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
                  "scienceSubtype": "POOR_WEATHER"
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ change type to C") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid, callProps =
        s"""
          largeProgram: {
             minPercentTime: 50
             toOActivation: STANDARD
             minPercentTotalTime: 25
             totalTime: { hours: 10.0 }
          }
        """.some
      ) *>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: SMALL_BODIES
                  type: {
                    classical: { }
                  }
                }
              }
            ) {
              proposal {
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
            "updateProposal": {
              "proposal": {
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

  test("⨯ missing proposal") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: { category: COSMOLOGY }
              }
            ) {
              proposal { category }
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
      addProposal(pi, pid) *>
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
              proposal { category }
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
      addProposal(pi, pid) *>
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
              proposal { category }
            }
          }
        """,
        expected =
          List("The specified Call for Proposals c-123 was not found.").asLeft
      )
    }
  }

  test("⨯ set mismatched cfp") {
    createCallForProposalsAs(staff, CallForProposalsType.DemoScience).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid) *>
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
                proposal { category }
              }
            }
          """,
          expected =
            List(s"The Call for Proposals $cid is a Demo Science call and cannot be used with a Queue proposal.").asLeft
        )
      }
    }
  }

  test("✓ set matching cfp") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid) *>
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
        ) *>
        assertIO(getProprietaryMonths(pi, pid), NonNegInt.unsafeFrom(12).some)
      }
    }
  }

  test("✓ set cfp on creation, default active period"):
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        assertIOBoolean(getActivePeriod(pi, pid).map(_ ===  DateInterval.between(LocalDate.of(2025, Month.FEBRUARY, 1), LocalDate.of(2025, Month.JULY, 31))))
      }
    }

  test("✓ set cfp later, default active period"):
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid) *>
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
                proposal { type { scienceSubtype } }
              }
            }
          """
        ) *>
        assertIOBoolean(getActivePeriod(pi, pid).map(_ ===  DateInterval.between(LocalDate.of(2025, Month.FEBRUARY, 1), LocalDate.of(2025, Month.JULY, 31))))
      }
    }

  test("⨯ invalid type change") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid) *>
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
                proposal { category }
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
                proposal { category }
              }
            }
          """,
          expected =
            List(s"The Call for Proposals $cid is a Regular Semester call and cannot be used with a Demo Science proposal.").asLeft
        )
      }
    }
  }

  test("✓ change call and type") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid) *>
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
                proposal { category }
              }
            }
          """
        ) *>
        assertIO(getProprietaryMonths(pi, pid), NonNegInt.unsafeFrom(12).some) *>
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
          ) *> assertIO(getProprietaryMonths(pi, pid), NonNegInt.unsafeFrom(3).some)
        }
      }
    }
  }

  test("✓ delete cfp") {
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        addProposal(pi, pid) *>
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
                proposal { category }
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

  test("⨯ update proposal in another user's program") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) *>
      expect(
        user = pi2,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                }
              }
            ) {
              proposal { category }
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
      addProposal(pi, pid) *>
      // the non-guest requirement gets caught before it even gets to the service.
      expect(
        user = guest,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                }
              }
            ) {
              proposal { category }
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
                category: COSMOLOGY
              }
            }
          ) {
            proposal { category }
          }
        }
      """,
      expected =
        List(s"Program $badPid does not exist, is not visible, or is ineligible for the requested operation.").asLeft
    )
  }

  test("⨯ update proposal in non-science program") {
    createProgramAs(pi).flatMap { pid =>
      setProgramReference(staff, pid, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""") >>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  category: COSMOLOGY
                }
              }
            ) {
              proposal { category }
            }
          }
        """,
        expected =
          List(s"Program $pid is of type Engineering. Only Science programs can have proposals.").asLeft
      )
    }
  }


  test("✓ changing proposal type clears reviewer and mentor") {
    for {
      pid      <- createProgramAs(pi, "Fast Turnaround to Queue Proposal")
      // Create FT proposal with reviewer and mentor
      coiId    <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi, education = EducationalStatus.PhD)
      mentorId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi, education = EducationalStatus.PhD)
      _ <- createFastTurnaroundProposalForUpdate(pi, pid, Some(coiId.toString), Some(mentorId.toString))
      // Now change the proposal type from FT to Queue
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateProposal(
              input: {
                programId: "$pid"
                SET: {
                  type: {
                    queue: {
                      toOActivation: NONE
                      minPercentTime: 50
                      partnerSplits: [
                        {
                          partner: US
                          percent: 100
                        }
                      ]
                    }
                  }
                }
              }
            ) {
              proposal {
                type {
                  scienceSubtype
                  ... on Queue {
                    minPercentTime
                  }
                }
              }
            }
          }
        """
      )
      // no reviewer or mentor after type change
      _ <- expect(
        user = pi,
        query = s"""
          query {
            program(programId: "$pid") {
              proposal {
                type {
                  scienceSubtype
                  ... on Queue {
                    minPercentTime
                  }
                  ... on FastTurnaround {
                    reviewer {
                      id
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
            "program": {
              "proposal": {
                "type": {
                  "scienceSubtype": "QUEUE",
                  "minPercentTime": 50
                }
              }
            }
          }
        """.asRight
      )
    } yield ()
  }

  test("✓ update fast turnaround proposal to set and clear reviewer and mentor") {
    for {
      pid        <- createProgramAs(pi, "Fast Turnaround Proposal")
      reviewerId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      mentorId   <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _          <- createFastTurnaroundProposalForUpdate(pi, pid, Some(reviewerId.toString), Some(mentorId.toString))
      _          <- updateFastTurnaroundProposal(pi, pid, None, None)
    } yield ()
  }

  test("✓ update fast turnaround proposal to change reviewer and mentor") {
    for {
      pid         <- createProgramAs(pi, "Fast Turnaround Proposal")
      reviewerId1 <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      mentorId1   <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      reviewerId2 <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      mentorId2   <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      // First create a FT proposal with initial reviewer and mentor
      _           <- createFastTurnaroundProposalForUpdate(pi, pid, Some(reviewerId1.toString), Some(mentorId1.toString))
      // change reviewer and mentor to different users
      _           <- updateFastTurnaroundProposal(pi, pid, Some(reviewerId2.toString), Some(mentorId2.toString))
    } yield ()
  }

  test("⨯ fast turnaround cannot set same user as reviewer and mentor") {
    for {
      pid    <- createProgramAs(pi, "Fast Turnaround Proposal")
      puId   <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _      <- createFastTurnaroundProposalForUpdate(pi, pid)
      _      <- updateFastTurnaroundProposalError(pi, pid, puId.toString)
    } yield ()
  }

  test("⨯ fast turnaround cannot update to reviewer from different program") {
    for {
      pid1       <- createProgramAs(pi, "p1")
      pid2       <- createProgramAs(pi, "p2")
      reviewerId <- addProgramUserAs(pi, pid2, role = ProgramUserRole.Coi) // reviewer in different program
      _          <- createFastTurnaroundProposalForUpdate(pi, pid1)
      _          <- expectOdbError(
                      user = pi,
                      query = s"""
                        mutation {
                          updateProposal(
                            input: {
                              programId: "$pid1"
                              SET: {
                                type: {
                                  fastTurnaround: {
                                    reviewerId: "$reviewerId"
                                  }
                                }
                              }
                            }
                          ) {
                            proposal { category }
                          }
                        }
                      """,
                      expected = {
                        case OdbError.InvalidArgument(Some("Reviewer must belong to the same program as the proposal.")) => // expected
                      }
                    )
    } yield ()
  }

  test("⨯ cannot update queue proposal to have reviewerId") {
    createProgramAs(pi, "Queue Proposal").flatMap { pid =>
      addProposal(pi, pid) *>
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
        expected = List("Unknown field(s) 'reviewerId' for input object value of type QueueInput in field 'updateProposal' of type 'Mutation'").asLeft
      )
    }
  }

}
