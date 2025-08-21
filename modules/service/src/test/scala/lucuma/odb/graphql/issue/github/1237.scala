// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.User

class GitHub_1237 extends OdbSuite {

  val pi         = TestUsers.Standard.pi(1, 101)
  val staff      = TestUsers.Standard.staff(4, 104)
  val validUsers = List(pi, staff)

  val sem24B   = Semester.unsafeFromString("2024B")
  val sem25A   = Semester.unsafeFromString("2025A")
  val ref24B01 = ProposalReference.fromString.unsafeGet("G-2024B-0001")
  val ref24B02 = ProposalReference.fromString.unsafeGet("G-2024B-0002")
  val ref24B03 = ProposalReference.fromString.unsafeGet("G-2024B-0003")

  private def switchCfp(user: User, pid: Program.Id, cid: CallForProposals.Id): IO[Option[ProposalReference]] =
    query(
      user,
      s"""
        mutation {
          updateProposal(
            input: {
              programId: "$pid"
              SET: {
                callId: "$cid"
              }
            }
          ) {
            proposal { reference { label } }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downFields("updateProposal", "proposal", "reference", "label")
        .focus
        .traverse { js =>
          js.hcursor.as[ProposalReference].leftMap(f => new RuntimeException(f.message)).liftTo[IO]
        }
    }

  test("switch proposal semester after submit") {
    for {
      cid24B <- createCallForProposalsAs(staff, semester = sem24B)
      cid25A <- createCallForProposalsAs(staff, semester = sem25A)

      pid1 <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid1, cid24B)
      _    <- addPartnerSplits(pi, pid1)
      _    <- addCoisAs(pi, pid1)
      ref1 <- submitProposal(pi, pid1)

      pid2 <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid2, cid25A)
      _    <- addPartnerSplits(pi, pid2)
      _    <- addCoisAs(pi, pid2)
      _    <- submitProposal(pi, pid2)
      ref2 <- switchCfp(staff, pid2, cid24B) // switches the semester to 2024B (updates index to 0002)

    } yield {
      assertEquals(ref1, ref24B01)
      assertEquals(ref2, ref24B02.some)
    }
  }

  test("switch proposal semester after unsubmit") {
    for {
      cid24B <- createCallForProposalsAs(staff, semester = sem24B)
      cid25A <- createCallForProposalsAs(staff, semester = sem25A)

      pid1 <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid1, cid24B)
      _    <- addPartnerSplits(pi, pid1)
      _    <- addCoisAs(pi, pid1)
      ref1 <- submitProposal(pi, pid1)

      pid2 <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid2, cid25A)
      _    <- addPartnerSplits(pi, pid2)
      _    <- addCoisAs(pi, pid2)
      _    <- submitProposal(pi, pid2)
      _    <- unsubmitProposal(pi, pid2)  // keeps the index 0001 of 2025A that was previously assigned
      ref2 <- switchCfp(pi, pid2, cid24B) // switches the semester to 2024B and deletes the index

    } yield {
      assertEquals(ref1, ref24B03)
      assertEquals(ref2, none[ProposalReference])
    }
  }
}
