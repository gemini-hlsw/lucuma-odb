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
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import org.http4s.Charset
import org.http4s.UrlForm
import org.http4s.dsl.Http4sDsl

class createUserInvitation extends OdbSuite {

  val partner = Partner.CA

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

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO).foreach { role =>
    test(s"invite ${role.toString.toLowerCase} (key)") {
      createProgramAs(pi).flatMap { pid =>
        createUserInvitationAs(pi, pid, role)
      }
    }
  }

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO).foreach { role =>
    test(s"invite ${role.toString.toLowerCase} (non-partner)") {
      createProgramAs(pi).flatMap { pid =>
        createUserInvitationAs(pi, pid, role, partnerLink = PartnerLink.HasNonPartner)
      }
    }
  }

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO).foreach { role =>
    test(s"invite ${role.toString.toLowerCase} (unspecified partner)") {
      createProgramAs(pi).flatMap { pid =>
        createUserInvitationAs(pi, pid, role, partnerLink = PartnerLink.HasUnspecifiedPartner)
      }
    }
  }

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO).foreach { pur =>
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
                partnerLink: {
                  partner: US
                }
              }
            ) {
              invitation {
                status
                issuer { id }
                recipientEmail
                redeemer { id }
                program { id }
                role
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

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO).foreach { pur =>
    test(s"coi can invite a ${pur.toString.toLowerCase} (metadata)") {
      val pid = for {
        pid <- createProgramAs(pi)
        _   <- createProgramAs(pi2) // this creates pi2
        _   <- linkCoiAs(pi, pi2.id -> pid, Partner.US)
      } yield pid

      pid.flatMap { pid =>
        expect(
          user = pi2,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programId: "$pid"
                recipientEmail: "$successRecipient"
                role: ${pur.tag.toUpperCase}
                partnerLink: {
                  partner: US
                }
              }
            ) {
              invitation {
                status
                issuer { id }
                recipientEmail
                redeemer { id }
                program { id }
                role
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
                      "id" : ${pi2.id}
                    },
                    "recipientEmail": $successRecipient,
                    "redeemer" : null,
                    "program" : {
                      "id" : $pid
                    },
                    "role" : ${pur: ProgramUserRole},
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

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO).foreach { pur =>
    test(s"readonly coi cannot invite a ${pur.toString.toLowerCase}") {
      val pid = for {
        pid <- createProgramAs(pi)
        _   <- createProgramAs(pi2) // this creates pi2
        _   <- linkObserverAs(pi, pi2.id -> pid, Partner.US)
      } yield pid

      pid.flatMap { pid =>
        expect(
          user = pi2,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programId: "$pid"
                recipientEmail: "$successRecipient"
                role: ${pur.tag.toUpperCase}
                partnerLink: {
                  partner: US
                }
              }
            ) {
              invitation {
                status
              }
            }
          }
          """,
          expected = Left(
            List("Specified program does not exist, or user is not the PI or a COI.")
          )
        )
      }
    }
  }

  test("invite support (metadata)") {
    List(ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).traverse: role =>
      createProgramAs(pi).flatMap { pid =>
        expect(
          user = staff,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programId: "$pid"
                recipientEmail: "$successRecipient"
                role: ${role.tag.toUpperCase}
              }
            ) {
              invitation {
                status
                issuer { id }
                recipientEmail
                redeemer { id }
                program { id }
                role
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
                    "role" : ${role: ProgramUserRole},
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
            partnerLink: {
              partner: US
            }
          }
        ) {
          key
        }
      }
      """,
      expected = Left(
        List(s"Specified program does not exist, or user is not the PI or a COI.")
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
              partnerLink: {
                partner: US
              }
            }
          ) {
            key
          }
        }
        """,
        expected = Left(
          List(s"Specified program does not exist, or user is not the PI or a COI.")
        )
      )
    }
  }

  test("pi can't invite a user to become the pi") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
        mutation {
          createUserInvitation(
            input: {
              programId: "$pid"
              recipientEmail: "$successRecipient"
              role: ${ProgramUserRole.Pi.tag.toUpperCase}
              partnerLink: {
                partner: US
              }
            }
          ) {
            key
          }
        }
        """,
        expected = List(s"Argument 'input' is invalid: Cannot create an invitation for the PI.").asLeft
      )
    }
  }

  List(guest, ngo).foreach: user =>
    test(s"${user.role.access} can't invite a user") {
      createProgramAs(user).flatMap { pid =>
        expectOdbError(
          user = user,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programId: "$pid"
                recipientEmail: "$successRecipient"
                role: ${ProgramUserRole.Coi.tag.toUpperCase}
                partnerLink: {
                  partner: US
                }
              }
            ) {
              key
            }
          }
          """,
          expected =
            val Id = user.id
            {
              case OdbError.NotAuthorized(Id, _) =>
            }
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
              role: ${ProgramUserRole.SupportPrimary.tag.toUpperCase}
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
