// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.literal._
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.UserInvitation
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserSupportType

class createUserInvitation extends OdbSuite {

  val pi  = TestUsers.Standard.pi(1, 101)
  val pi2 = TestUsers.Standard.pi(2, 201)

  val validUsers = List(pi, pi2).toList

  List(ProgramUserRole.Coi, ProgramUserRole.Observer).foreach { pur =>
    test(s"invite ${pur.toString.toLowerCase} (key)") {
      createProgramAs(pi).flatMap { pid =>
        query(
          user = pi,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programId: "$pid"
                role: ${pur.tag.toUpperCase}
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
  }

  List(ProgramUserRole.Coi, ProgramUserRole.Observer).foreach { pur =>
    test(s"invite ${pur.toString.toLowerCase} (metadata)") {
      createProgramAs(pi).flatMap { pid =>
        expect(
          user = pi,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programId: "$pid"
                role: ${pur.tag.toUpperCase}
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
                    "role" : ${pur: ProgramUserRole},
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
  }

  test("invite staff support (metadata)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              role: ${ProgramUserRole.Support.tag.toUpperCase}
              supportType: ${ProgramUserSupportType.Staff.tag.toUpperCase()}
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
                  "role" : ${ProgramUserRole.Support: ProgramUserRole},
                  "supportType" : ${ProgramUserSupportType.Staff: ProgramUserSupportType},
                  "supportPartner" : null
                }
              }
            }
          """
        )
      )
    }
  }

  test("invite partner support (metadata)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              role: ${ProgramUserRole.Support.tag.toUpperCase}
              supportType: ${ProgramUserSupportType.Partner.tag.toUpperCase()}
              supportPartner: CA
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
                  "role" : ${ProgramUserRole.Support: ProgramUserRole},
                  "supportType" : ${ProgramUserSupportType.Partner: ProgramUserSupportType},
                  "supportPartner" : "CA"
                }
              }
            }
          """
        )
      )
    }
  }

  test("can't invite partner support without a partner") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              role: ${ProgramUserRole.Support.tag.toUpperCase}
              supportType: ${ProgramUserSupportType.Partner.tag.toUpperCase()}
            }
          ) {
            invitation {
              status
            }
          }
        }
        """,
        expected = Left(List(
          "Argument 'input' is invalid: Invalid combination of role, support type, and partner."
        ))
      )
    }
  }

  test("can't invite a user if it's not your program") {
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

  test("can't invite a user to a non-existent program") {
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
