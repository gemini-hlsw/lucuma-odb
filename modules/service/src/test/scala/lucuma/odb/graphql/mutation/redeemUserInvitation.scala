// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.model.Partner
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.UserInvitation

class redeemUserInvitation extends OdbSuite {

  val partner = Partner.Ca

  val pi      = TestUsers.Standard.pi(1, 101)
  val pi2     = TestUsers.Standard.pi(2, 201)
  val guest   = TestUsers.guest(3)
  val staff   = TestUsers.Standard.staff(4, 401)
  val admin   = TestUsers.Standard.admin(5, 501)
  val service = TestUsers.service(6)
  val ngo     = TestUsers.Standard.ngo(7, 701, partner)

  val validUsers = List(pi, pi2, guest, staff, admin, service, ngo).toList

  def redeem(inv: UserInvitation, accept: Boolean = true): String =
    s"""
      mutation {
        redeemUserInvitation(input: { 
          key: "${UserInvitation.fromString.reverseGet(inv)}"
          accept: $accept
        }) {
          invitation {
            id
            status
            issuer {
              id
            }
            redeemer {
              id
            }
            program {
              users {
                role
                userId
              }
            }
          }
        }
      }
    """

  test("redeem an invitation") {
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        expect(
          user = pi2,
          query = redeem(inv),
          expected = Right(json"""
            {
              "redeemUserInvitation" : {
                "invitation" : {
                  "id" : ${UserInvitation.Id.fromString.reverseGet(inv.id)},
                  "status" : ${UserInvitation.Status.Redeemed},
                  "issuer" : {
                    "id" : ${pi.id}
                  },
                  "redeemer" : {
                    "id" : ${pi2.id}
                  },
                  "program" : {
                    "users" : [
                      {
                        "role" : ${ProgramUserRole.Coi.tag.toUpperCase},
                        "userId" : ${pi2.id}
                      }
                    ]
                  }                
                }
              }
            }
          """)
        )
      }
    }
  }

  test("can't redeem an invitation that has been revoked") {
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        revokeUserInvitationAs(pi, inv.id) >>
        expect(
          user = pi2,
          query = redeem(inv),
          expected = Left(List("Invitation is invalid, or has already been accepted, declined, or revoked."))
        )
      }
    }
  }

  test("guests can't redeem invitations") {
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        expect(
          user = guest,
          query = redeem(inv),
          expected = Left(List("Guest users cannot redeem user invitations."))
        )
      }
    }
  }

  test("service users can't redeem invitations") {
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        expect(
          user = service,
          query = redeem(inv),
          expected = Left(List("Service users cannot redeem user invitations."))
        )
      }
    }
  }

  test("can't redeem an invitation twice") {
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        redeemUserInvitationAs(pi2, inv) >>
        expect(
          user = pi2,
          query = redeem(inv),
          expected = Left(List("Invitation is invalid, or has already been accepted, declined, or revoked."))
        )
      }
    }
  }

  test("can't redeem your own invitation") {
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        expect(
          user = pi,
          query = redeem(inv),
          expected = Left(List("Invitation is invalid, or has already been accepted, declined, or revoked."))
        )
      }
    }
  }

  test("redeeming an invitation works if you aready have that role, but it's a no-op in terms of linking") {
    createProgramAs(pi).flatMap { pid =>
      linkAs(pi, pi2.id, pid, ProgramUserRole.Coi, None, None) >>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        expect(
          user = pi2,
          query = redeem(inv),
          expected = Left(List("You are already in the specified role; no action taken."))
        )
      }
    }
  }

}