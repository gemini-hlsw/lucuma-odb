// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.ProgramUser
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import org.http4s.Charset
import org.http4s.UrlForm
import org.http4s.dsl.Http4sDsl

class createUserInvitation extends OdbSuite:

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

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO, ProgramUserRole.External).foreach: role =>
    test(s"invite ${role.toString.toLowerCase} (key)"):
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid, role).flatMap: mid =>
          createUserInvitationAs(pi, mid)

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO, ProgramUserRole.External).foreach: role =>
    test(s"invite ${role.toString.toLowerCase} (non-partner)"):
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid, role, PartnerLink.HasNonPartner).flatMap: mid =>
          createUserInvitationAs(pi, mid)

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO, ProgramUserRole.External).foreach: role =>
    test(s"invite ${role.toString.toLowerCase} (unspecified partner)"):
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid, role, PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
          createUserInvitationAs(pi, mid)

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO, ProgramUserRole.External).foreach: pur =>
    test(s"invite ${pur.toString.toLowerCase} (metadata)"):
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid, pur, PartnerLink.HasPartner(Partner.US)).flatMap: mid =>
          expect(
            user = pi,
            query = s"""
            mutation {
              createUserInvitation(
                input: {
                  programUserId: "$mid"
                  recipientEmail: "$successRecipient"
                }
              ) {
                invitation {
                  status
                  issuer { id }
                  recipientEmail
                  programUser {
                    id
                    role
                    user { id }
                    program { id }
                  }
                  email {
                    senderEmail
                    status
                  }
                }
              }
            }
            """,
            expected = json"""
              {
                "createUserInvitation" : {
                  "invitation" : {
                    "status" : ${InvitationStatus.Pending},
                    "issuer" : { "id" : ${pi.id} },
                    "recipientEmail": $successRecipient,
                    "programUser" : {
                      "id" : $mid,
                      "role": ${pur: ProgramUserRole},
                      "user": null,
                      "program": {
                        "id": $pid
                      }
                    },
                    "email": {
                      "senderEmail": ${emailConfig.invitationFrom},
                      "status": ${EmailStatus.Queued}
                    }
                  }
                }
              }
            """.asRight
          )

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO, ProgramUserRole.External).foreach: pur =>
    test(s"coi can invite a ${pur.toString.toLowerCase} (metadata)"):
      val pidrid = for
        pid  <- createProgramAs(pi)
        _    <- createProgramAs(pi2) // this creates pi2
        rid2 <- addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasPartner(Partner.US))
        _    <- linkUserAs(pi, rid2, pi2.id)
        rid3 <- addProgramUserAs(pi, pid, pur)
      yield (pid, rid3)

      pidrid.flatMap: (pid, mid) =>
        expect(
          user = pi2,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programUserId: "$mid"
                recipientEmail: "$successRecipient"
              }
            ) {
              invitation {
                status
                issuer { id }
                recipientEmail
                programUser {
                  id
                  role
                  user { id }
                  program { id }
                }
                email {
                  senderEmail
                  status
                }
              }
            }
          }
          """,
          expected =
            json"""
              {
                "createUserInvitation" : {
                  "invitation" : {
                    "status" : ${InvitationStatus.Pending},
                    "issuer" : { "id" : ${pi2.id} },
                    "recipientEmail": $successRecipient,
                    "programUser": {
                      "id": $mid,
                      "role" : ${pur: ProgramUserRole},
                      "user": null,
                      "program" : { "id" : $pid }
                    },
                    "email": {
                      "senderEmail": ${emailConfig.invitationFrom},
                      "status": ${EmailStatus.Queued}
                    }
                  }
                }
              }
            """.asRight
        )

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO, ProgramUserRole.External).foreach: pur =>
    test(s"readonly coi cannot invite a ${pur.toString.toLowerCase}"):
      val mid = for
        pid  <- createProgramAs(pi)
        _    <- createProgramAs(pi2) // this creates pi2
        rid2 <- addProgramUserAs(pi, pid, ProgramUserRole.CoiRO)
        _    <- linkUserAs(pi, rid2, pi2.id)
        rid3 <- addProgramUserAs(pi, pid, pur)  // add as PI, try to invite as CoiRO below
      yield rid3

      mid.flatMap: mid =>
        expect(
          user = pi2,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programUserId: "$mid"
                recipientEmail: "$successRecipient"
              }
            ) {
              invitation {
                status
              }
            }
          }
          """,
          expected = List(
            "Specified program does not exist, or user is not the PI or COI."
          ).asLeft
        )

  test("invite support (metadata)"):
    List(ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).traverse: role =>
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(staff, pid, role).flatMap: mid =>
          expect(
            user  = staff,
            query = s"""
            mutation {
              createUserInvitation(
                input: {
                  programUserId: "$mid"
                  recipientEmail: "$successRecipient"
                }
              ) {
                invitation {
                  status
                  issuer { id }
                  recipientEmail
                  programUser {
                    id
                    role
                    program { id }
                  }
                  email {
                    senderEmail
                    status
                  }
                }
              }
            }
            """,
            expected =
              json"""
                {
                  "createUserInvitation" : {
                    "invitation" : {
                      "status" : ${InvitationStatus.Pending},
                      "issuer" : { "id" : ${staff.id} },
                      "recipientEmail": $successRecipient,
                      "programUser": {
                        "id": $mid,
                        "role": ${role: ProgramUserRole},
                        "program" : { "id" : $pid }
                      },
                      "email": {
                        "senderEmail": ${emailConfig.invitationFrom},
                        "status": ${EmailStatus.Queued}
                      }
                    }
                  }
                }
              """.asRight
          )

  def createUserInvitationQuery(mid: ProgramUser.Id): String =
    s"""
      mutation {
        createUserInvitation(
          input: {
            programUserId: "$mid"
            recipientEmail: "$successRecipient"
          }
        ) {
          key
        }
      }
    """

  test("can't invite a user with a non-existent program user id"):
    expect(
      user     = pi,
      query    = createUserInvitationQuery(ProgramUser.Id.parse("m-ffff").get),
      expected = List(
        s"ProgramUser m-ffff does not exist, or is ineligible for the requested operation."
      ).asLeft
    )

  test("pi can't invite a user if it's not their program"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        expect(
          user     = pi2,
          query    = createUserInvitationQuery(mid),
          expected = List(
            s"Specified program does not exist, or user is not the PI or COI."
          ).asLeft
        )

  test("guest can't invite a coi"):
    createProgramAs(guest).flatMap: pid =>
      addProgramUserAs(staff, pid).flatMap: mid =>
        expect(
          user     = guest,
          query    = createUserInvitationQuery(mid),
          expected = List(
            s"Guest users cannot create invitations."
          ).asLeft
       )

  test("ngo without allocation can't add a coi"):
    createProgramAs(ngo).flatMap: pid =>
      addProgramUserAs(staff, pid).flatMap: mid =>
        expect(
          user     = ngo,
          query    = createUserInvitationQuery(mid),
          expected = List(
            s"NGO users can't create invitations."
          ).asLeft
        )

  test("staff, admin, service can create an invitation for any program"):
    createProgramAs(pi).flatMap: pid =>
      List(staff, admin, service).traverse: user =>
        addProgramUserAs(pi, pid).flatMap: mid =>
          createUserInvitationAs(user, mid)

  test("Bad response from sending email results in failure"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid, ProgramUserRole.SupportPrimary).flatMap: mid =>
        expect(
          user  = staff,
          query = s"""
          mutation {
            createUserInvitation(
              input: {
                programUserId: "$mid"
                recipientEmail: "$badResponseRecipient"
              }
            ) {
              invitation {
                status
              }
            }
          }
          """,
          expected = List(
            "Unexpected status '400 Bad Request' while attempting to send email."
          ).asLeft
        )

  def invitationsQuery(pid: Program.Id): String =
    s"""
      query {
        programUsers(
          WHERE: {
            program: { id : { EQ: "$pid" } }
            role: { NEQ: PI }
          }
        ) {
          matches {
            invitations {
              status
            }
          }
        }
      }
    """

  test("program user invitations"):
    for
      pid  <- createProgramAs(pi)
      mid  <- addProgramUserAs(pi, pid)
      inv0 <- createUserInvitationAs(pi, mid)
      _    <- expect(
        user     = pi,
        query    = invitationsQuery(pid),
        expected =
          json"""
            {
              "programUsers": {
                "matches": [
                  {
                    "invitations": [ { "status": "PENDING" } ]
                  }
                ]
              }
            }
          """.asRight
        )
    yield ()

  test("one pending invitation per user"):
    for
      pid <- createProgramAs(pi)
      mid <- addProgramUserAs(pi, pid)
      inv <- createUserInvitationAs(pi, mid)
        _ <- interceptOdbError(createUserInvitationAs(pi, mid)):
               case OdbError.UpdateFailed(_) => // ok
    yield ()

  test("reject, invite again"):
    createProgramAs(pi2).>>
      for
        pid  <- createProgramAs(pi)
        mid  <- addProgramUserAs(pi, pid)
        inv0 <- createUserInvitationAs(pi, mid)
        _    <- redeemUserInvitationAs(pi2, inv0, false)
        inv1 <- createUserInvitationAs(pi, mid)
        _    <- redeemUserInvitationAs(pi2, inv1, true)
        _    <- expect(
          user     = pi,
          query    = invitationsQuery(pid),
          expected =
            json"""
              {
                "programUsers": {
                  "matches": [
                    {
                      "invitations": [
                       { "status": "DECLINED" },
                       { "status": "REDEEMED" }
                      ]
                    }
                  ]
                }
              }
            """.asRight
          )
      yield ()

  test("revoke, invite again"):
    createProgramAs(pi2).>>
      for
        pid  <- createProgramAs(pi)
        mid  <- addProgramUserAs(pi, pid)
        inv0 <- createUserInvitationAs(pi, mid)
        _    <- revokeUserInvitationAs(pi, inv0.id)
        inv1 <- createUserInvitationAs(pi, mid)
        _    <- redeemUserInvitationAs(pi2, inv1, true)
        _    <- expect(
          user     = pi,
          query    = invitationsQuery(pid),
          expected =
            json"""
              {
                "programUsers": {
                  "matches": [
                    {
                      "invitations": [
                       { "status": "REVOKED" },
                       { "status": "REDEEMED" }
                      ]
                    }
                  ]
                }
              }
            """.asRight
          )
      yield ()

  test("can't invite after linked"):
    for
      pid <- createProgramAs(pi)
      mid <- addProgramUserAs(pi, pid)
      _   <- linkUserAs(pi, mid, pi2.id)
      _   <- interceptOdbError(createUserInvitationAs(pi, mid)):
               case OdbError.InvalidProgramUser(mid, _) => // ok
    yield ()

  test(s"Delete a program user after invitation acceptance."):
    createProgramAs(pi2).>>
    for
      pid <- createProgramAs(pi)
      mid <- addProgramUserAs(pi, pid)
      inv <- createUserInvitationAs(pi, mid)
      _   <- redeemUserInvitationAs(pi2, inv, true)
      _   <- deleteProgramUserAs(pi, mid)
      _   <- expect(
          user     = pi,
          query    = invitationsQuery(pid),
          expected =
            json"""
              {
                "programUsers": {
                  "matches": [ ]
                }
              }
            """.asRight
      )
    yield ()
