// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal._
import lucuma.core.model.Partner
import lucuma.core.model.User
import lucuma.odb.data.UserInvitation

class revokeUserInvitation extends OdbSuite {

  val partner = Partner.Ca

  val pi      = TestUsers.Standard.pi(1, 101)
  val pi2     = TestUsers.Standard.pi(2, 201)
  val guest   = TestUsers.guest(3)
  val staff   = TestUsers.Standard.staff(4, 401)
  val admin   = TestUsers.Standard.admin(5, 501)
  val service = TestUsers.service(6)
  val ngo     = TestUsers.Standard.ngo(7, 701, partner)

  val validUsers = List(pi, pi2, guest, staff, admin, service, ngo).toList

  def revoke(id: UserInvitation.Id): String =
    s"""
      mutation {
        revokeUserInvitation(input: { 
          id: "${UserInvitation.Id.fromString.reverseGet(id)}"
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

  test("revoke an invitation") {
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        expect(
          user = pi,
          query = revoke(inv.id),
          expected = Right(json"""
            {
              "revokeUserInvitation" : {
                "invitation" : {
                  "id" : ${UserInvitation.Id.fromString.reverseGet(inv.id)},
                  "status" : ${UserInvitation.Status.Revoked},
                  "issuer" : {
                    "id" : ${pi.id}
                  },
                  "redeemer" : null,
                  "program" : {
                    "users" : []
                  }                
                }
              }
            }
          """)
        )
      }
    }
  }

  List(true, false).foreach { accept => 
    test(s"can't revoke an invitation that was already ${if accept then "accepted" else "delined"}") {    
      createProgramAs(pi).flatMap { pid =>
        createUserInvitationAs(pi, pid).flatMap { inv =>
          redeemUserInvitationAs(pi2, inv, accept) >>
          expect(
            user = pi,
            query = revoke(inv.id),
            expected = Left(List("Invitation does not exist, is no longer pending, or was issued by someone else."))
          )
        }
      }
    }
  }

  test("can't revoke an invitation twice") {    
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        revokeUserInvitationAs(pi, inv.id) >>
        expect(
          user = pi,
          query = revoke(inv.id),
          expected = Left(List("Invitation does not exist, is no longer pending, or was issued by someone else."))
        )
      }
    }
  }


  test("guest can't revoke an invitation") {
    createProgramAs(pi).flatMap { pid =>
      createUserInvitationAs(pi, pid).flatMap { inv =>
        expect(
          user = guest,
          query = revoke(inv.id),
          expected = Left(List("Guest users cannot revoke invitations."))
        )
      }
    }
  }

  List(pi2, ngo).foreach { u =>
    test(s"non-superuser (${u.role.access}) can't revoke someone else's invitation") {
      createProgramAs(pi).flatMap { pid =>
        createUserInvitationAs(pi, pid).flatMap { inv =>
          expect(
            user = u,
            query = revoke(inv.id),
            expected = Left(List("Invitation does not exist, is no longer pending, or was issued by someone else."))
          )
        }
      }
    }
  }

  List(admin, staff, service).foreach { u =>
    test(s"superuser (${u.role.access}) *can* revoke someone else's invitation") {
      createProgramAs(pi).flatMap { pid =>
        createUserInvitationAs(pi, pid).flatMap { inv =>
          revokeUserInvitationAs(u, inv.id)
        }
      }
    }
  }

}