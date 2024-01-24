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
  val ref2024B1  = ProposalReference.FromString.unsafeGet("G-2024B-0001")
  val ref2024B2  = ProposalReference.FromString.unsafeGet("G-2024B-0002")

  val sem2025A   = Semester.unsafeFromString("2025A")
  val ref2025A1  = ProposalReference.FromString.unsafeGet("G-2025A-0001")

  def submitProposal(pid: Program.Id, s: Semester): IO[ProposalReference] =
      query(pi, s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposalStatus: SUBMITTED,
                  semester: "${s.format}"
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                proposalReference
              }
            }
          }
        """
      ).flatMap { js =>
        js.hcursor
          .downField("updatePrograms")
          .downField("programs")
          .downArray
          .downField("proposalReference")
          .as[ProposalReference]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
      }

  test("submit proposals") {
    for {
      pid0 <- createProgramAs(pi)
      _    <- addProposal(pi, pid0)
      ref0 <- submitProposal(pid0, sem2024B)

      pid1 <- createProgramAs(pi)
      _    <- addProposal(pi, pid1)
      ref1 <- submitProposal(pid1, sem2024B)

      pid2 <- createProgramAs(pi)
      _    <- addProposal(pi, pid2)
      ref2 <- submitProposal(pid2, sem2025A)
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
          program(proposalReference: "G-2024B-001") {
            proposalReference
          }
        }
      """,
      expected = Right(
        json"""
          {
            "program": {
              "proposalReference": $ref2024B1
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
}
