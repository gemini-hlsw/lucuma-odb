// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.enums.InvitationStatus
import lucuma.core.enums.Partner
import lucuma.core.model.ProgramUser
import lucuma.core.model.User
import lucuma.core.model.UserInvitation
import lucuma.odb.data.OdbError

class revokeUserInvitation extends OdbSuite:

  val partner = Partner.CA

  val pi      = TestUsers.Standard.pi(1, 101)
  val pi2     = TestUsers.Standard.pi(2, 201)
  val guest   = TestUsers.guest(3)
  val staff   = TestUsers.Standard.staff(4, 401)
  val admin   = TestUsers.Standard.admin(5, 501)
  val service = TestUsers.service(6)
  val ngo     = TestUsers.Standard.ngo(7, 701, partner)

  val validUsers = List(pi, pi2, guest, staff, admin, service, ngo).toList

  override val httpRequestHandler = invitationEmailRequestHandler

  def revoke(id: UserInvitation.Id): String =
    s"""
      mutation {
        revokeUserInvitation(input: {
          id: "${UserInvitation.Id.fromString.reverseGet(id)}"
        }) {
          invitation {
            status
            issuer { id }
            programUser {
              id
              user { id }
              program {
                users {
                  role
                  user { id }
                }
              }
            }
          }
        }
      }
    """

  test("revoke an invitation"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          expect(
            user     = pi,
            query    = revoke(inv.id),
            expected = json"""
              {
                "revokeUserInvitation" : {
                  "invitation" : {
                    "status" : ${InvitationStatus.Revoked},
                    "issuer" : { "id" : ${pi.id} },
                    "programUser": {
                      "id": $mid,
                      "user": null,
                      "program": {
                        "users": [
                          {
                            "role": "COI",
                            "user": null
                          }
                        ]
                      }
                    }
                  }
                }
              }
            """.asRight
          )

  test("create, query, then revoke an invitation"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          query(
            user = pi,
            query = s"""
              query {
                program(programId: "$pid") {
                  userInvitations {
                    programUser {
                      id
                    }
                  }
                }
              }
            """
          ).flatMap: _ =>
            expect(
              user     = pi,
              query    = revoke(inv.id),
              expected = json"""
                {
                  "revokeUserInvitation" : {
                    "invitation" : {
                      "status" : ${InvitationStatus.Revoked},
                      "issuer" : { "id" : ${pi.id} },
                      "programUser": {
                        "id": $mid,
                        "user": null,
                        "program": {
                          "users": [
                            {
                              "role": "COI",
                              "user": null
                            }
                          ]
                        }
                      }
                    }
                  }
                }
              """.asRight
            )

  val badInvitation: PartialFunction[OdbError, Unit] =
    case OdbError.InvitationError(_, Some("Invitation does not exist, is no longer pending, or was issued by someone else.")) => ()

  List(true, false).foreach: accept =>
    test(s"can't revoke an invitation that was already ${if accept then "accepted" else "delined"}"):
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid).flatMap: mid =>
          createUserInvitationAs(pi, mid).flatMap: inv =>
            redeemUserInvitationAs(pi2, inv, accept) >>
            expectOdbError(
              user     = pi,
              query    = revoke(inv.id),
              expected = badInvitation
            )

  test("can't revoke an invitation twice"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          revokeUserInvitationAs(pi, inv.id) >>
          expectOdbError(
            user     = pi,
            query    = revoke(inv.id),
            expected = badInvitation
          )

  test("guest can't revoke an invitation"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          val gid = guest.id
          expectOdbError(
            user     = guest,
            query    = revoke(inv.id),
            expected =
              case OdbError.NotAuthorized(`gid`, Some("Guest users cannot revoke invitations.")) => ()
          )

  List(pi2, ngo).foreach: u =>
    test(s"non-superuser (${u.role.access}) can't revoke someone else's invitation"):
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid).flatMap: mid =>
          createUserInvitationAs(pi, mid).flatMap: inv =>
            expectOdbError(
              user     = u,
              query    = revoke(inv.id),
              expected = badInvitation
            )

  List(admin, staff, service).foreach: u =>
    test(s"superuser (${u.role.access}) *can* revoke someone else's invitation"):
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid).flatMap: mid =>
          createUserInvitationAs(pi, mid).flatMap: inv =>
            revokeUserInvitationAs(u, inv.id)
