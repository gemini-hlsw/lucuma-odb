// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError

class addProgramUser extends OdbSuite:
  val partner = Partner.CA

  val pi1      = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi1, pi2, pi3, ngo, staff, admin, guest, service)

  override val httpRequestHandler = invitationEmailRequestHandler

  test("addProgramUser"):
    createProgramAs(pi1).flatMap: pid =>
      expect(
        user     = pi1,
        query    = s"""
          mutation {
            addProgramUser(
              input: {
                programId: "$pid"
                role: COI
                SET: {
                  partnerLink: { partner: CA }
                  preferredProfile: {
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
                preferredProfile {
                  givenName
                  familyName
                  creditName
                  email
                }
                educationalStatus
                thesis
                gender
                hasDataAccess
              }
            }
          }
        """,
        expected = json"""
          {
            "addProgramUser": {
              "programUser": {
                "role": "COI",
                "partnerLink": {
                  "linkType": "HAS_PARTNER",
                  "partner": "CA"
                },
                "preferredProfile": {
                  "givenName": "Gavrilo",
                  "familyName": "Princip",
                  "creditName": "Гаврило Принцип",
                  "email": "gprincip@mladabosna.org"
                },
                "educationalStatus": "GRAD_STUDENT",
                "thesis": false,
                "gender": "MALE",
                "hasDataAccess": true
              }
            }
          }
        """.asRight
      )

  def testInvalidInput(partnerLinkInput: String): IO[Unit] =
    createProgramAs(pi1).flatMap: pid =>
      expect(
        user = pi1,
        query = s"""
          mutation {
            addProgramUser(input: {
              programId: "$pid"
              role: ${ProgramUserRole.Coi.tag.toScreamingSnakeCase}
              SET: {
                partnerLink: {
                  $partnerLinkInput
                }
              }
            }) {
              programUser { id }
            }
          }
        """,
        expected = List("Argument 'input.SET.partnerLink' is invalid: Specify either 'linkType' (as `HAS_NON_PARTNER` or `HAS_UNSPECIFIED_PARTNER`) or 'partner'.").asLeft
      )

  test("[general] (empty link)"):
    testInvalidInput("")

  test("[general] (missing partner)"):
    testInvalidInput(
      """
        linkType: HAS_PARTNER
      """.stripMargin
    )

  test("[general] (conflicting)"):
    testInvalidInput(
      """
        linkType: HAS_NON_PARTNER
        partner: US
      """.stripMargin
    )

  def addProgramUserQuery(pid: Program.Id, role: ProgramUserRole): String =
    s"""
      mutation {
        addProgramUser(
          input: {
            programId: "$pid"
            role: ${role.tag.toScreamingSnakeCase}
          }
        ) { programUser { id } }
      }
    """

  private def partnerLinkFor(role: ProgramUserRole): PartnerLink =
    if (role === ProgramUserRole.SupportPrimary || role === ProgramUserRole.SupportSecondary) PartnerLink.HasUnspecifiedPartner
    else PartnerLink.HasPartner(Partner.US)

  // What can a Guest do?

  Enumerated[ProgramUserRole].all.foreach: role =>
    test(s"Guest can't add $role (NotAuthorized)."):
      interceptOdbError {
        for
          _   <- createUsers(guest, pi2, admin)
          pid <- createProgramAs(guest)
          _   <- addProgramUserAs(guest, pid, role, partnerLinkFor(role))
        yield ()
      } {
        case OdbError.NotAuthorized(guest.id, _) => // this is what we expect
        case OdbError.InvalidArgument(Some("Argument 'input' is invalid: PIs are added at program creation time.")) => // ok
      }

  // What can a PI do?

  test("pi can't add a pi"):
    createProgramAs(pi1).flatMap: pid =>
      expect(
        user     = pi1,
        query    = addProgramUserQuery(pid, ProgramUserRole.Pi),
        expected = List(
          s"Argument 'input' is invalid: PIs are added at program creation time."
        ).asLeft
      )

  List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.External).foreach: link =>
    test(s"PI can add $link"):
      for
        _   <- createUsers(pi1, pi2)
        pid <- createProgramAs(pi1)
        mid <- addProgramUserAs(pi1, pid, link, PartnerLink.HasPartner(Partner.CA))
        _   <- assertIO(listProgramUsersAs(pi1, pid), List((mid, link, None)))
      yield ()

  List(ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).foreach: role =>
    test(s"PI can't add $role (NotAuthorized)."):
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2)
          pid <- createProgramAs(pi1)
          _   <- addProgramUserAs(pi1, pid, role, partnerLinkFor(role))
        yield ()
      } {
        case OdbError.NotAuthorized(pi1.id, _) => // this is what we expect
      }

  // What can a Coi do?

  List(ProgramUserRole.CoiRO, ProgramUserRole.External).foreach: role =>
    test(s"Coi can add $role."):
      for
        _    <- createUsers(pi1, pi2)
        pid  <- createProgramAs(pi1)
        mid  <- addProgramUserAs(pi1, pid, ProgramUserRole.Coi, PartnerLink.HasPartner(Partner.AR))
        _    <- linkUserAs(pi1, mid, pi2.id)
        rid2 <- addProgramUserAs(pi1, pid, role)
        _    <- assertIO(
                  listProgramUsersAs(pi1, pid),
                  List((mid, ProgramUserRole.Coi, pi2.id.some), (rid2, role, none))
                )
      yield ()

  List(ProgramUserRole.Coi, ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).foreach: role =>
    test(s"Coi can't add $role (NotAuthorized)."):
      interceptOdbError {
        for
          _    <- createUsers(pi1, pi2)
          pid  <- createProgramAs(pi1)
          mid  <- addProgramUserAs(pi1, pid, ProgramUserRole.Coi)
          _    <- linkUserAs(pi1, mid, pi2.id)
          _    <- addProgramUserAs(pi2, pid, role, partnerLinkFor(role))
        yield ()
      } {
        case OdbError.NotAuthorized(pi2.id, _) => // this is what we expect
      }

  // What can an External user do?
  Enumerated[ProgramUserRole].all.foreach: role =>
    test(s"External users can't add $role (NotAuthorized)."):
      interceptOdbError {
        for
          _    <- createUsers(pi1, pi2)
          pid  <- createProgramAs(pi1)
          mid  <- addProgramUserAs(pi1, pid, ProgramUserRole.External)
          _    <- linkUserAs(pi1, mid, pi2.id)
          _    <- addProgramUserAs(pi2, pid, role, partnerLinkFor(role))
        yield ()
      } {
        case OdbError.NotAuthorized(pi2.id, _) => // this is what we expect
        case OdbError.InvalidArgument(Some("Argument 'input' is invalid: PIs are added at program creation time.")) => // ok
      }

  // What can NGO user do?

  List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.External).foreach: role =>
    test(s"Ngo (CA) can add $role with allocated time."):
      for
        _   <- createUsers(pi1, pi2, admin, ngo)
        pid <- createProgramAs(pi1)
        _   <- setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, TimeSpan.Max) // so ngo can see the program
        _   <- addProgramUserAs(ngo, pid, role, partnerLinkFor(role))
      yield ()

  List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.External).foreach: role =>
    test(s"Ngo (CA) can't add $role without allocated time."):
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, admin, ngo)
          pid <- createProgramAs(pi1)
          _   <- addProgramUserAs(ngo, pid, role, partnerLinkFor(role))
        yield ()
      } {
        case OdbError.NotAuthorized(ngo.id, _) => // this is what we expect
      }

  List(ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).foreach: role =>
    test(s"Ngo (CA) can't add $role (NotAuthorized)."):
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, admin, ngo)
          pid <- createProgramAs(pi1)
          _   <- setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, TimeSpan.Max) // so ngo can see the program
          _   <- addProgramUserAs(ngo, pid, role, partnerLinkFor(role))
        yield ()
      } {
        case OdbError.NotAuthorized(ngo.id, _) => // this is what we expect
      }

  // What can superusers do?

  List(staff, admin, service).foreach: u =>
    List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).foreach: role =>
      test(s"${u.role.access} can add $role."):
        for
          _   <- createUsers(pi1, pi2, u)
          pid <- createProgramAs(pi1)
          _   <- addProgramUserAs(u, pid, role, partnerLinkFor(role))
        yield ()

  test(s"Nobody can add another PI."):
    interceptOdbError {
      for
        _   <- createUsers(pi1, admin)
        pid <- createProgramAs(pi1)
        _   <- addProgramUserAs(admin, pid, ProgramUserRole.Pi, partnerLinkFor(ProgramUserRole.Pi))
      yield ()
    } {
      case OdbError.InvalidArgument(Some("Argument 'input' is invalid: PIs are added at program creation time.")) => // ok
    }
