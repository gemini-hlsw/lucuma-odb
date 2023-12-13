// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.literal._
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.UserInvitation

class createUserInvitation extends OdbSuite {

  val pi  = TestUsers.Standard.pi(1, 101)
  val pi2 = TestUsers.Standard.pi(2, 201)

  val validUsers = List(pi, pi2).toList

  test("invite a coi (key)") {
    createProgramAs(pi).flatMap { pid =>
      query(
        user = pi,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              role: ${ProgramUserRole.Coi.tag.toUpperCase}
            }
          ) {
            key
          }
        }
        """
      ).map { js =>
        js.hcursor
          .downField("key")
          .as[UserInvitation] // just be sure we can decode
      }
    }
  }

  test("invite a coi (metadata)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              role: ${ProgramUserRole.Coi.tag.toUpperCase}
            }
          ) {
            invitation {
              status
              issuer { id }
              redeemer { id }
              program { id }
              role
              supportType
              supportPartner
            }
          }
        }
        """,
        expected = Right(
          json"""
            {
              "createUserInvitation" : {
                "invitation" : {
                  "status" : ${UserInvitation.Status.Pending},
                  "issuer" : {
                    "id" : ${pi.id}
                  },
                  "redeemer" : null,
                  "program" : {
                    "id" : $pid
                  },
                  "role" : ${ProgramUserRole.Coi: ProgramUserRole},
                  "supportType" : null,
                  "supportPartner" : null
                }
              }
            }
          """
        )
      )
    }
  }

  test("can't invite a coi if it's not your program") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi2,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              role: ${ProgramUserRole.Coi.tag.toUpperCase}
            }
          ) {
            key
          }
        }
        """,
        expected = Left(
          List(s"Specified program does not exist or user ${pi2.id} is not the PI.")
        )
      )
    }
  }

  test("can't invite a coi to a non-existent program") {
    expect(
      user = pi,
      query = s"""
      mutation {
        createUserInvitation(
          input: {
            programId: "p-ffff"
            role: ${ProgramUserRole.Coi.tag.toUpperCase}
          }
        ) {
          key
        }
      }
      """,
      expected = Left(
        List(s"Specified program does not exist or user ${pi.id} is not the PI.")
      )
    )
  }

}
