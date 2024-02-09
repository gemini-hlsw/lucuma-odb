// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.odb.data.ProposalReference


class reference extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)

  val validUsers = List(pi, guest, service)

  val sem2024B   = Semester.unsafeFromString("2024B")
  val ref2024B1  = ProposalReference.fromString.unsafeGet("G-2024B-0001")
  val ref2024B2  = ProposalReference.fromString.unsafeGet("G-2024B-0002")
  val ref2024B3  = ProposalReference.fromString.unsafeGet("G-2024B-0003")

  val sem2025A   = Semester.unsafeFromString("2025A")
  val ref2025A1  = ProposalReference.fromString.unsafeGet("G-2025A-0001")

  val sem2025B   = Semester.unsafeFromString("2025B")
  val ref2025B1  = ProposalReference.fromString.unsafeGet("G-2025B-0001")
  val ref2025B2  = ProposalReference.fromString.unsafeGet("G-2025B-0002")

  test("submit proposals") {
    for {
      pid0 <- createProgramAs(pi)
      _    <- addProposal(pi, pid0)
      ref0 <- submitProposal(pi, pid0, sem2024B.some)

      pid1 <- createProgramAs(pi)
      _    <- addProposal(pi, pid1)
      ref1 <- submitProposal(pi, pid1, sem2024B.some)

      pid2 <- createProgramAs(pi)
      _    <- addProposal(pi, pid2)
      ref2 <- submitProposal(pi, pid2, sem2025A.some)
    } yield {
      assertEquals(ref0, ref2024B1)
      assertEquals(ref1, ref2024B2)
      assertEquals(ref2, ref2025A1)
    }
  }

  test("lookup via proposal ref") {
    expect(
      user  = pi,
      query = s"""
        query {
          program(proposalReference: "G-2024B-0001") {
            programReference
            proposalReference
          }
        }
      """,
      expected = Right(
        json"""
          {
            "program": {
              "programReference": null,
              "proposalReference": ${ref2024B1.format}
            }
          }
        """
      )
    )
  }

  test("lookup, no pid, no ref") {
    expect(
      user  = pi,
      query = s"""
        query {
          program {
            proposalReference
          }
        }
      """,
      expected = Right(
        json"""
          {
            "program": null
          }
        """
      )
    )
  }

  test("select via WHERE semester") {
    expect(
      user = pi,
      query = s"""
        query {
          programs(
            WHERE: {
              semester: {
                EQ: "2024B"
              }
            }
          ) {
            matches {
              proposalReference
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "programs": {
              "matches": [
                 {
                   "proposalReference": $ref2024B1
                 },
                 {
                   "proposalReference": $ref2024B2
                 }
              ]
            }
          }
        """
      )
    )
  }

  test("select via WHERE semester IS_NULL") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          query {
            programs(
              WHERE: {
                semester: {
                  IS_NULL: true
                }
              }
            ) {
              matches {
                id
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "programs": {
                "matches": [
                  {
                    "id": $pid
                  }
                ]
              }
            }
          """
        )
      )
    }
  }

  test("set semester on create") {
    expect(
      user = pi,
      query = s"""
        mutation {
          createProgram(
            input: {
               SET: {
                  semester: "2025A"
               }
            }
          ) {
            program { semester }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "createProgram": {
              "program": {
                "semester": "2025A"
              }
            }
          }
        """
      )
    )
  }

  test("set semester on create, then submit") {
    val createWithSemester = query(pi, s"""
        mutation {
          createProgram(
            input: {
               SET: {
                  semester: "2025B"
               }
            }
          ) {
            program { id }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downFields("createProgram", "program", "id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

    val res = for {
      pid <- createWithSemester
      _   <- addProposal(pi, pid)
      ref <- submitProposal(pi, pid, none) // no semester
    } yield ref

    assertIO(res, ref2025B1)
  }

  test("cannot unset semester after submit") {
    fetchPid(pi, ref2025B1).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
            mutation {
              updatePrograms(
                input: {
                  SET: {
                    semester: null
                  }
                  WHERE: {
                    semester: {
                      EQ: "2025B"
                    }
                  }
                }
              ) {
                programs {
                  proposalReference
                }
              }
            }
        """,
        expected = Left(List(
          s"Submitted program $pid must be associated with a semester."
        ))
      )
    }
  }

  test("unset semester in unsubmitted program") {
    val createWithSemester = query(pi, s"""
        mutation {
          createProgram(
            input: {
               SET: {
                  semester: "2025B"
               }
            }
          ) {
            program { id }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downFields("createProgram", "program", "id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

    createWithSemester.flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  semester: null
                }
                WHERE: {
                  id: { EQ: "$pid" }
                }
              }
            ) {
              programs {
                semester
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
                    "semester": null
                  }
                ]
              }
            }
          """
        )
      )
    }
  }

  test("unsubmit, resubmit, same reference") {
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
                proposalReference: {
                  EQ: "${ref2024B2.format}"
                }
              }
            }
          ) {
            programs {
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
                  "proposalStatus": "NOT_SUBMITTED"
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
                proposalStatus: SUBMITTED
              }
              WHERE: {
                proposalReference: {
                  EQ: "${ref2024B2.format}"
                }
              }
            }
          ) {
            programs {
              proposalReference
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
                  "proposalReference": ${ref2024B2.format}
                }
              ]
            }
          }
        """
      )
    )
  }

  test("change semester, changes reference") {
    // G-2025A-0001 -> assign semester 2024B
    // G-2024B-0001 and 00002 already taken, so we get G-2024B-0003
    expect(
      user = pi,
      query = s"""
        mutation {
          updatePrograms(
            input: {
              SET: {
                semester: "2024B"
              }
              WHERE: {
                proposalReference: {
                  EQ: "${ref2025A1.format}"
                }
              }
            }
          ) {
            programs {
              proposalReference
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
                  "proposalReference": ${ref2024B3.format}
                }
              ]
            }
          }
        """
      )
    )
  }

  test("where semesterIndex") {
    expect(
      user = pi,
      query = s"""
        query {
          programs(
            WHERE: {
              semester: {
                EQ: "2024B"
              }
              semesterIndex: {
                GT: 1
              }
            }
          ) {
            matches {
              proposalReference
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "programs" : {
              "matches": [
                {
                  "proposalReference": ${ref2024B2.format}
                },
                {
                  "proposalReference": ${ref2024B3.format}
                }
              ]
            }
          }
        """
      )
    )
  }

  test("where semester") {
    expect(
      user = pi,
      query = s"""
        query {
          programs(
            WHERE: {
              semester: {
                EQ: "2024B"
              }
            }
          ) {
            matches {
              proposalReference
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "programs" : {
              "matches": [
                {
                  "proposalReference": ${ref2024B1.format}
                },
                {
                  "proposalReference": ${ref2024B2.format}
                },
                {
                  "proposalReference": ${ref2024B3.format}
                }
              ]
            }
          }
        """
      )
    )
  }

  test("set semester in the distant future") {
    // G-2025A-0001 -> assign semester 2024B
    // G-2024B-0001 and 00002 already taken, so we get G-2024B-0003
    expect(
      user = pi,
      query = s"""
        mutation {
          updatePrograms(
            input: {
              SET: {
                semester: "9999B"
              }
              WHERE: {
                proposalReference: {
                  EQ: "${ref2024B3.format}"
                }
              }
            }
          ) {
            programs {
              proposalReference
            }
          }
        }
        """,
      expected = Left(List(
        "The maximum semester is capped at the current year +1 (9999B specified)."
      ))
    )
  }

}
