// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import fs2.text.utf8
import io.circe.Json
import io.circe.literal.*
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.EmailStatus
import lucuma.core.enums.InvitationStatus
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import org.http4s.Charset
import org.http4s.UrlForm
import org.http4s.dsl.Http4sDsl

class createUserInvitation extends OdbSuite {

  val partner = Partner.Ca

  val pi      = TestUsers.Standard.pi(1, 101)
  val pi2     = TestUsers.Standard.pi(2, 201)
  val guest   = TestUsers.guest(3)
  val staff   = TestUsers.Standard.staff(4, 401)
  val admin   = TestUsers.Standard.admin(5, 501)
  val service = TestUsers.service(6)
  val ngo     = TestUsers.Standard.ngo(7, 701, partner)

  val validUsers = List(pi, pi2, guest, staff, admin, service, ngo).toList

  val successRecipient = EmailAddress.unsafeFrom("bob@dobbs.com")
  val badResponseRecipient = EmailAddress.unsafeFrom("bad@wolf.com")

  override val httpRequestHandler =
    req => {
      val dsl = Http4sDsl[IO]
      import dsl.*

      val recipient =
        req.body.through(utf8.decode).compile.toList
          .map(l => UrlForm.decodeString(Charset.`UTF-8`)(l.head).toOption.get.get("to").headOption.get)
          .map(EmailAddress.unsafeFrom)
      val response =
        recipient.flatMap(recip =>
          if (recip === badResponseRecipient)
            BadRequest("whatever")
          else {
            val sio = UUIDGen[IO].randomUUID.map(uuid => 
              Json.obj(
                "id"      -> s"<$uuid>".asJson,
                "message" -> "Queued".asJson
              ).toString
            )
            sio.flatMap(s => Ok(s))
          }
        )
      Resource.eval(response)
    }

  List(ProgramUserRole.Coi, ProgramUserRole.Observer).foreach { role =>
    test(s"invite ${role.toString.toLowerCase} (key)") {
      createProgramAs(pi).flatMap { pid =>
        createUserInvitationAs(pi, pid, role)
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
                recipientEmail: "$successRecipient"
                role: ${pur.tag.toUpperCase}
              }
            ) {
              invitation {
                status
                issuer { id }
                recipientEmail
                redeemer { id }
                program { id }
                role
                supportType
                supportPartner
                email {
                  senderEmail
                  status
                }
              }
            }
          }
          """,
          expected = Right(
            json"""
              {
                "createUserInvitation" : {
                  "invitation" : {
                    "status" : ${InvitationStatus.Pending},
                    "issuer" : {
                      "id" : ${pi.id}
                    },
                    "recipientEmail": $successRecipient,
                    "redeemer" : null,
                    "program" : {
                      "id" : $pid
                    },
                    "role" : ${pur: ProgramUserRole},
                    "supportType" : null,
                    "supportPartner" : null,
                    "email": {
                      "senderEmail": ${emailConfig.invitationFrom},
                      "status": ${EmailStatus.Queued}
                    }
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
        user = staff,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              recipientEmail: "$successRecipient"
              role: ${ProgramUserRole.Support.tag.toUpperCase}
              supportType: ${ProgramUserSupportType.Staff.tag.toUpperCase()}
            }
          ) {
            invitation {
              status
              issuer { id }
              recipientEmail
              redeemer { id }
              program { id }
              role
              supportType
              supportPartner
              email {
                senderEmail
                status
              }
            }
          }
        }
        """,
        expected = Right(
          json"""
            {
              "createUserInvitation" : {
                "invitation" : {
                  "status" : ${InvitationStatus.Pending},
                  "issuer" : {
                    "id" : ${staff.id}
                  },
                  "recipientEmail": $successRecipient,
                  "redeemer" : null,
                  "program" : {
                    "id" : $pid
                  },
                  "role" : ${ProgramUserRole.Support: ProgramUserRole},
                  "supportType" : ${ProgramUserSupportType.Staff: ProgramUserSupportType},
                  "supportPartner" : null,
                  "email": {
                    "senderEmail": ${emailConfig.invitationFrom},
                    "status": ${EmailStatus.Queued}
                  }
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
        user = admin,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              recipientEmail: "$successRecipient"
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
              email {
                senderEmail
                status
              }
            }
          }
        }
        """,
        expected = Right(
          json"""
            {
              "createUserInvitation" : {
                "invitation" : {
                  "status" : ${InvitationStatus.Pending},
                  "issuer" : {
                    "id" : ${admin.id}
                  },
                  "redeemer" : null,
                  "program" : {
                    "id" : $pid
                  },
                  "role" : ${ProgramUserRole.Support: ProgramUserRole},
                  "supportType" : ${ProgramUserSupportType.Partner: ProgramUserSupportType},
                  "supportPartner" : "CA",
                  "email": {
                    "senderEmail": ${emailConfig.invitationFrom},
                    "status": ${EmailStatus.Queued}
                  }
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
              recipientEmail: "$successRecipient"
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

  test("can't invite a user to a non-existent program") {
    expect(
      user = pi,
      query = s"""
      mutation {
        createUserInvitation(
          input: {
            programId: "p-ffff"
            recipientEmail: "$successRecipient"
            role: ${ProgramUserRole.Coi.tag.toUpperCase}
          }
        ) {
          key
        }
      }
      """,
      expected = Left(
        List(s"Specified program does not exist, or user is not the PI.")
      )
    )
  }


  test("pi can't invite a user if it's not their program") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi2,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              recipientEmail: "$successRecipient"
              role: ${ProgramUserRole.Coi.tag.toUpperCase}
            }
          ) {
            key
          }
        }
        """,
        expected = Left(
          List(s"Specified program does not exist, or user is not the PI.")
        )
      )
    }
  }

  test("guest can't invite a user") {
    createProgramAs(guest).flatMap { pid =>
      expect(
        user = guest,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              recipientEmail: "$successRecipient"
              role: ${ProgramUserRole.Coi.tag.toUpperCase}
            }
          ) {
            key
          }
        }
        """,
        expected = Left(
          List(s"Guest users cannot create invitations.")
        )
      )
    }
  }

  test("staff, admin, service can create an invitation for any program") {
    createProgramAs(pi).flatMap { pid =>
      List(staff, admin, service).traverse { user =>
        createUserInvitationAs(user, pid)
      }
    }
  }

  test("ngo user can create invitation if time is allocated") {
    createProgramAs(pi).flatMap { pid =>
      setAllocationAs(admin, pid, Tag(partner.tag), TimeSpan.FromHours.getOption(1).get) >>
      createUserInvitationAs(ngo, pid, ProgramUserRole.Support, Some(ProgramUserSupportType.Partner), Some(Tag(partner.tag)))
    }
  }

  test("ngo can't create an invitation if no time allocated") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = ngo,
        query = s"""
          mutation {
            createUserInvitation(
              input: {
                programId: "$pid"
                recipientEmail: "bob@dobbs.com"
                role: SUPPORT
                supportType: PARTNER
                supportPartner: ${partner.tag.toUpperCase}
              }
            ) {
              key
            }
          }
          """,
        expected = Left(List(
          "Specified program does not exist, or has no partner-allocated time."
        ))
      )
    }
  }

  test("Bad response from sending email results in failure") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = staff,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              recipientEmail: "$badResponseRecipient"
              role: ${ProgramUserRole.Support.tag.toUpperCase}
              supportType: ${ProgramUserSupportType.Staff.tag.toUpperCase()}
            }
          ) {
            invitation {
              status
            }
          }
        }
        """,
        expected = Left(List(
          "Unexpected status '400 Bad Request' while attempting to send email."
        ))
      )
    }
  }

}
