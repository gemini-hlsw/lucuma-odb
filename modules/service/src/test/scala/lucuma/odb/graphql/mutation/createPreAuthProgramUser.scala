// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.data.Ior
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.User
import lucuma.core.model.UserInvitation

class createPreAuthProgramUser extends OdbSuite:
  val pi  = TestUsers.Standard.pi(1, 30)
  val pa0 = TestUsers.Standard.pi(100L, 100L)
  val pa1 = TestUsers.Standard.pi(101L, 101L)

  val validUsers = List(pi, pa0, pa1)

  override val httpRequestHandler = invitationEmailRequestHandler

  test("createPreAuthProgramUser"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            createPreAuthProgramUser(
              input: {
                orcidId: "${TestUsers.orcidId(100L).value}"
                programId: "$pid"
                role: COI
                SET: {
                  partnerLink: { partner: CA }
                  fallbackProfile: {
                    givenName: "Gavrilo"
                    familyName: "Princip"
                    creditName: "Гаврило Принцип"
                    email: "gprincip@mladabosna.org"
                  }
                  educationalStatus: GRAD_STUDENT
                  thesis: false
                  gender: MALE
                }
              }
            ) {
              programUser {
                role
                partnerLink {
                  linkType
                  ...on HasPartner {
                    partner
                  }
                }
                fallbackProfile {
                  givenName
                  familyName
                  creditName
                  email
                }
                educationalStatus
                thesis
                gender
              }
            }
          }
        """,
        expected = json"""
          {
            "createPreAuthProgramUser": {
              "programUser": {
                "role": "COI",
                "partnerLink": {
                  "linkType": "HAS_PARTNER",
                  "partner": "CA"
                },
                "fallbackProfile": {
                  "givenName": "Gavrilo",
                  "familyName": "Princip",
                  "creditName": "Гаврило Принцип",
                  "email": "gprincip@mladabosna.org"
                },
                "educationalStatus": "GRAD_STUDENT",
                "thesis": false,
                "gender": "MALE"
              }
            }
          }
        """.asRight
      )

  test("createPreAuthProgramUser: illegal role"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            createPreAuthProgramUser(
              input: {
                orcidId: "${TestUsers.orcidId(100L).value}"
                programId: "$pid"
                role: PI
              }
            ) {
              programUser {
                user { id }
              }
            }
          }
        """,
        expected = List(
          "Argument 'input' is invalid: Only co-investigators who have not accepted an invitation may be linked via this method, not PI"
        ).asLeft
      )

  test("createPreAuthProgramUser: redeem after added"):
    for
      pid <- createProgramAs(pi)
      inv <- createUserInvitationAs(pi, pid, ProgramUserRole.Coi, PartnerLink.HasPartner(Partner.CL))
      uid <- query(user = pi,
               s"""
                 mutation {
                   createPreAuthProgramUser(
                     input: {
                       orcidId: "${TestUsers.orcidId(101L).value}"
                       programId: "$pid"
                       role: COI
                       SET: {
                          partnerLink: { partner: CA }
                       }
                     }
                   ) {
                     programUser {
                       user { id }
                     }
                   }
                }
               """
             ).map: json =>
               json.hcursor.downFields("createPreAuthProgramUser", "programUser", "user", "id").require[User.Id]
          // Since the user was added explicitly with createPreAuthProgramUser,
          // when they accept the invitation there will already be a link.
      _   <- expectIor(
               user = pa1,
               query = s"""
                 mutation {
                   redeemUserInvitation(input: {
                     key: "${UserInvitation.fromString.reverseGet(inv)}"
                     accept: true
                   }) {
                     invitation {
                       program {
                         users {
                           user { id }
                         }
                       }
                     }
                   }
                 }
               """,
               expected = Ior.both(
                 List("You are already in the specified role; no action taken."),
                 json"""
                   {
                     "redeemUserInvitation": {
                       "invitation": {
                         "program": {
                           "users": [
                             {
                               "user": {
                                 "id": ${pa1.id}
                               }
                             }
                           ]
                         }
                       }
                     }
                   }
                 """
               )
             )
    yield ()