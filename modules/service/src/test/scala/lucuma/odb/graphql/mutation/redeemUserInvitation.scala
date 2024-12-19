// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.InvitationStatus
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.User
import lucuma.core.model.UserInvitation

class redeemUserInvitation extends OdbSuite:

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

  def redeem(inv: UserInvitation, accept: Boolean = true): String =
    s"""
      mutation {
        redeemUserInvitation(input: {
          key: "${UserInvitation.fromString.reverseGet(inv)}"
          accept: $accept
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

  test("redeem an invitation"):
    createProgramAs(pi2) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          expect(
            user  = pi2,
            query = redeem(inv),
            expected = json"""
              {
                "redeemUserInvitation" : {
                  "invitation" : {
                    "status" : ${InvitationStatus.Redeemed},
                    "issuer" : { "id" : ${pi.id} },
                    "programUser": {
                      "id": $mid,
                      "user": { "id": ${pi2.id} },
                      "program" : {
                        "users" : [
                          {
                            "role" : ${ProgramUserRole.Coi.tag.toUpperCase},
                            "user" : {
                              "id": ${pi2.id}
                            }
                          }
                        ]
                      }
                    }
                  }
                }
              }
            """.asRight
          )

  test("redeem an invitation without partner"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          expect(
            user  = pi2,
            query = redeem(inv),
            expected = json"""
              {
                "redeemUserInvitation" : {
                  "invitation" : {
                    "status" : ${InvitationStatus.Redeemed},
                    "issuer" : { "id" : ${pi.id} },
                    "programUser": {
                      "id": $mid,
                      "user": { "id": ${pi2.id} },
                      "program" : {
                        "users" : [
                          {
                            "role" : ${ProgramUserRole.Coi.tag.toUpperCase},
                            "user" : {
                              "id": ${pi2.id}
                            }
                          }
                        ]
                      }
                    }
                  }
                }
              }
            """.asRight
          )

  test("can't redeem an invitation that has been revoked"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          revokeUserInvitationAs(pi, inv.id) >>
          expect(
            user     = pi2,
            query    = redeem(inv),
            expected = List(
              "Invitation is invalid, or has already been accepted, declined, or revoked."
            ).asLeft
          )

  test("guests can't redeem invitations"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          expect(
            user     = guest,
            query    = redeem(inv),
            expected = List(
              "Guest users cannot redeem user invitations."
            ).asLeft
          )

  test("service users can't redeem invitations"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          expect(
            user     = service,
            query    = redeem(inv),
            expected = List(
              "Service users cannot redeem user invitations."
            ).asLeft
          )

  test("can't redeem an invitation twice"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          redeemUserInvitationAs(pi2, inv) >>
          expect(
            user     = pi2,
            query    = redeem(inv),
            expected = List(
              "Invitation is invalid, or has already been accepted, declined, or revoked."
            ).asLeft
          )

  test("can't redeem your own invitation"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          expect(
            user     = pi,
            query    = redeem(inv),
            expected = List(
              "Invitation is invalid, or has already been accepted, declined, or revoked."
            ).asLeft
          )

  test("redeeming an invitation works if you aready have that role, but it's a no-op in terms of linking"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid).flatMap: mid =>
        createUserInvitationAs(pi, mid).flatMap: inv =>
          linkUserAs(pi, mid, pi2.id) >>
          expect(
            user     = pi2,
            query    = redeem(inv),
            expected = List(
              "You are already in the specified role; no action taken."
            ).asLeft
          )