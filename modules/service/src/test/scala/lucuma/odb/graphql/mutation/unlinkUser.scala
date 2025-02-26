// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.eq.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProgramUser
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError

class unlinkUser extends OdbSuite:

  val pi1      = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi1, pi2, pi3, ngo, staff, admin, guest, service)

  def unlinkAs(
    user: User,
    mid:  ProgramUser.Id
  ): IO[Boolean] =
    query(
      user = user,
      query = s"""
        mutation {
          unlinkUser(input: {
            programUserId: "$mid"
          }) {
            result
          }
        }
      """
    ).map: j =>
      j.hcursor.downFields("unlinkUser", "result").require[Boolean]

  // General rules

  test("Unlink should return false if link doesn't exist."):
    for
      _   <- createUsers(pi1, pi2)
      pid <- createProgramAs(pi1)
      mid <- addProgramUserAs(pi1, pid)
      _   <- assertIO(unlinkAs(pi1, mid), false)
    yield ()

  test("Unlink should fail if program isn't accessible to the user."):
    for
      _   <- createUsers(pi1, pi2, pi3)
      pid <- createProgramAs(pi1)
      mid <- addProgramUserAs(pi1, pid, partnerLink = PartnerLink.HasPartner(Partner.US))
      _   <- linkUserAs(pi1, mid, pi2.id)
      _   <- interceptOdbError(unlinkAs(pi3, mid)):
               case OdbError.NotAuthorized(_, _) => // ok
    yield ()

  private def partnerLinkFor(role: ProgramUserRole): PartnerLink =
    if (role === ProgramUserRole.SupportPrimary || role === ProgramUserRole.SupportSecondary) PartnerLink.HasUnspecifiedPartner
    else PartnerLink.HasPartner(Partner.US)

  // What can a Guest do?

  Enumerated[ProgramUserRole].all.foreach: role =>
    test(s"Guest can't unlink $role (NotAuthorized)."):
      interceptOdbError {
        for
          _   <- createUsers(guest, pi2, admin)
          pid <- createProgramAs(guest)
          mid <- addProgramUserAs(admin, pid, role, partnerLinkFor(role))
          _   <- linkUserAs(admin, mid, pi2.id)
          _   <- unlinkAs(guest, mid)
        yield ()
      } {
        case OdbError.NotAuthorized(guest.id, _) => // this is what we expect
        case OdbError.InvalidArgument(Some("Argument 'input' is invalid: PIs are added at program creation time.")) => // ok
      }

  // What can a PI do?

  List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.External).foreach: link =>
    test(s"PI can unlink $link"):
      for
        _   <- createUsers(pi1, pi2)
        pid <- createProgramAs(pi1)
        mid <- addProgramUserAs(pi1, pid, link, PartnerLink.HasPartner(Partner.CA))
        _   <- linkUserAs(pi1, mid, pi2.id)
        _   <- assertIO(unlinkAs(pi1, mid), true)
      yield ()

  List(ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).foreach: role =>
    test(s"PI can't unlink $role (NotAuthorized)."):
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, admin)
          pid <- createProgramAs(pi1)
          mid <- addProgramUserAs(admin, pid, role, partnerLinkFor(role))
          _   <- linkUserAs(admin, mid, pi2.id)
          _   <- unlinkAs(pi1, mid)
        yield ()
      } {
        case OdbError.NotAuthorized(pi1.id, _) => // this is what we expect
      }

  // What can a Coi do?

  List(ProgramUserRole.CoiRO, ProgramUserRole.External).foreach: role =>
    test(s"Coi can unlink $role."):
      for
        _    <- createUsers(pi1, pi2, pi3)
        pid  <- createProgramAs(pi1)
        rid2 <- addProgramUserAs(pi1, pid, ProgramUserRole.Coi, PartnerLink.HasPartner(Partner.AR))
        _    <- linkUserAs(pi1, rid2, pi2.id)
        rid3 <- addProgramUserAs(pi1, pid, role)
        _    <- linkUserAs(pi1, rid3, pi3.id)
        _    <- assertIO(unlinkAs(pi2, rid3), true)
      yield ()

  List(ProgramUserRole.Coi, ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).foreach: role =>
    test(s"Coi can't unlink $role (NotAuthorized)."):
      interceptOdbError {
        for
          _    <- createUsers(pi1, pi2, pi3, admin)
          pid  <- createProgramAs(pi1)
          rid2 <- addProgramUserAs(admin, pid, role, partnerLinkFor(role))
          _    <- linkUserAs(admin, rid2, pi2.id)
          rid3 <- addProgramUserAs(admin, pid, ProgramUserRole.Coi, PartnerLink.HasPartner(Partner.US))
          _    <- linkUserAs(admin, rid3, pi3.id)
          _    <- unlinkAs(pi3, rid2)
        yield ()
      } {
        case OdbError.NotAuthorized(pi3.id, _) => // this is what we expect
      }

  // What can NGO user do?

  List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.External).foreach: role =>
    test(s"Ngo (CA) can unlink $role with allocated time."):
      for
        _   <- createUsers(pi1, pi2, admin, ngo)
        pid <- createProgramAs(pi1)
        mid <- addProgramUserAs(admin, pid, role, partnerLinkFor(role))
        _   <- linkUserAs(admin, mid, pi2.id)
        _   <- setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, TimeSpan.Max) // so ngo can see the program
        _   <- assertIO(unlinkAs(ngo, mid), true)
      yield ()

  List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.External).foreach: role =>
    test(s"Ngo (CA) can't unlink $role without allocated time."):
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, admin, ngo)
          pid <- createProgramAs(pi1)
          mid <- addProgramUserAs(admin, pid, role, partnerLinkFor(role))
          _   <- linkUserAs(admin, mid, pi2.id)
          _   <- unlinkAs(ngo, mid)
        yield ()
      } {
        case OdbError.NotAuthorized(ngo.id, _) => // this is what we expect
      }

  List(ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).foreach: role =>
    test(s"Ngo (CA) can't unlink $role (NotAuthorized)."):
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, admin, ngo)
          pid <- createProgramAs(pi1)
          mid <- addProgramUserAs(admin, pid, role, partnerLinkFor(role))
          _   <- linkUserAs(admin, mid, pi2.id)
          _   <- setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, TimeSpan.Max) // so ngo can see the program
          _   <- unlinkAs(ngo, mid)
        yield ()
      } {
        case OdbError.NotAuthorized(ngo.id, _) => // this is what we expect
      }

  // What can superusers do?

  List(staff, admin, service).foreach: u =>
    List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.External, ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary).foreach: role =>
      test(s"${u.role.access} can unlink $role."):
        for
          _   <- createUsers(pi1, pi2, u)
          pid <- createProgramAs(pi1)
          mid <- addProgramUserAs(admin, pid, role, partnerLinkFor(role))
          _   <- linkUserAs(admin, mid, pi2.id)
          _   <- assertIO(unlinkAs(u, mid), true)
        yield ()

  test(s"Nobody can unlink the PI."):
    interceptOdbError {
      for
        _   <- createUsers(pi1, admin)
        pid <- createProgramAs(pi1)
        mid <- piProgramUserIdAs(pi1, pid)
        _   <- assertIO(unlinkAs(admin, mid), false)
      yield ()
    } {
      case OdbError.UpdateFailed(Some("PIs are fixed at program creation time.")) => // expected
    }