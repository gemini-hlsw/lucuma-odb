// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.User

class fastTurnaroundTests extends OdbSuite {

  val pi    = TestUsers.Standard.pi(1, 101)
  val coi   = TestUsers.Standard.pi(2, 102)
  val staff = TestUsers.Standard.staff(3, 103)

  val validUsers = List(pi, coi, staff)

  test("✓ create FT proposal with null reviewer by default") {
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
                      toOActivation: NONE
                      minPercentTime: 50
                      piAffiliation: US
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
                  "reviewer": null
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("✓ fast turnaround proposal returns null mentor by default") {
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
                      toOActivation: NONE
                      minPercentTime: 50
                      piAffiliation: US
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

  test("⨯ cannot assign mentor role to non-PhD user") {
    for {
      pid <- createProgramAs(pi, "My Fast Turnaround Proposal")
      
      // Create FT proposal first
      _ <- query(
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
                      piAffiliation: US
                    }
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
        """
      )
      
      // Add COI with undergraduate education (non-PhD)
      puId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi, education = EducationalStatus.UndergradStudent)
      
      // Try to set mentor role on the non-PhD user - should fail with service layer validation
      _ <- expect(
        user = staff,
        query = s"""
          mutation {
            updateProgramUsers(
              input: {
                SET: {
                  ftSupportRole: MENTOR
                }
                WHERE: {
                  id: { EQ: "$puId" }
                }
              }
            ) {
              programUsers {
                id
              }
            }
          }
        """,
        expected = List("Users with mentor role must have PhD educational status").asLeft
      )
    } yield ()
  }

  test("✓ can assign mentor role to PhD user") {
    for {
      pid <- createProgramAs(pi, "My Fast Turnaround Proposal")
      
      // Create FT proposal first
      _ <- query(
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
                      piAffiliation: US
                    }
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
        """
      )
      
      // Add COI with PhD education
      puId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi, education = EducationalStatus.PhD)
      
      // Try to set mentor role on the PhD user - should succeed
      _ <- expect(
        user = staff,
        query = s"""
          mutation {
            updateProgramUsers(
              input: {
                SET: {
                  ftSupportRole: MENTOR
                }
                WHERE: {
                  id: { EQ: "$puId" }
                }
              }
            ) {
              programUsers {
                id
                ftSupportRole
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProgramUsers": {
              "programUsers": [
                {
                  "id": $puId,
                  "ftSupportRole": "MENTOR"
                }
              ]
            }
          }
        """.asRight
      )
    } yield ()
  }


  test("⨯ cannot assign FT support role to non-FT proposal") {
    for {
      pid <- createProgramAs(pi, "My Queue Proposal")
      
      // Create Queue proposal first
      _ <- query(
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
                }
              }
            }
          }
        """
      )
      
      // Add COI with PhD education
      puId <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi, education = EducationalStatus.PhD)
      
      // Try to set mentor role on Queue proposal - should fail
      _ <- expect(
        user = staff,
        query = s"""
          mutation {
            updateProgramUsers(
              input: {
                SET: {
                  ftSupportRole: MENTOR
                }
                WHERE: {
                  id: { EQ: "$puId" }
                }
              }
            ) {
              programUsers {
                id
                ftSupportRole
              }
            }
          }
        """,
        expected = List("FT support roles can only be assigned to Fast Turnaround proposals").asLeft
      )
    } yield ()
  }
}