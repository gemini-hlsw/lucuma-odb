// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import cats.syntax.eq.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProgramUser
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError


class changeProgramUserRole extends OdbSuite:
  val partner  = Partner.CA
  val pi1      = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi1, pi2, pi3, ngo, staff, admin, guest, service)

  private def changeUserRoleQuery(puid: ProgramUser.Id, newRole: ProgramUserRole): String =
    s"""
      mutation {
        changeProgramUserRole(
          input: {
            programUserId: "$puid"
            newRole: ${newRole.tag.toScreamingSnakeCase}
          }
        ) {
          programUser {
            role
          }
        }
      }
    """

  private def changeProgramUserRoleResult(newRole: ProgramUserRole): Json =
    json"""
      {
        "changeProgramUserRole": {
          "programUser": {
            "role": $newRole
          }
        }
      }
    """

  private def partnerLinkFor(role: ProgramUserRole): PartnerLink =
    if (role === ProgramUserRole.SupportPrimary || role === ProgramUserRole.SupportSecondary) PartnerLink.HasUnspecifiedPartner
    else PartnerLink.HasPartner(Partner.US)

  test("canonical use case"):
    createProgramAs(pi1).flatMap: pid =>
      addProgramUserAs(pi1, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        expect(
          user     = pi1,
          query    = changeUserRoleQuery(mid, ProgramUserRole.Coi),
          expected = changeProgramUserRoleResult(ProgramUserRole.Coi).asRight
        )


  // What can a PI do?

  test("Cannot switch a COI to PI"):
    createUsers(pi2) >>
    createProgramAs(pi1).flatMap: pid =>
      addProgramUserAs(pi1, pid, ProgramUserRole.Coi).flatMap: mid =>
        linkUserAs(pi1, mid, pi2.id) >>
        expect(
          user     = pi1,
          query    = changeUserRoleQuery(mid, ProgramUserRole.Pi),
          expected = List(s"Argument 'input' is invalid: PIs are added at program creation time.").asLeft
        )

  List(
    ProgramUserRole.CoiRO -> ProgramUserRole.Coi,
    ProgramUserRole.Coi   -> ProgramUserRole.CoiRO
  ).foreach: (from, to) =>
    test(s"PI can change $from to $to"):
      createProgramAs(pi1).flatMap: pid =>
        addProgramUserAs(pi1, pid, from).flatMap: mid =>
          expect(
            user     = pi1,
            query    = changeUserRoleQuery(mid, to),
            expected = changeProgramUserRoleResult(to).asRight
          )

  List(
    ProgramUserRole.CoiRO -> ProgramUserRole.SupportPrimary,
    ProgramUserRole.CoiRO -> ProgramUserRole.SupportSecondary,
    ProgramUserRole.Coi   -> ProgramUserRole.SupportPrimary,
    ProgramUserRole.Coi   -> ProgramUserRole.SupportSecondary,
    ProgramUserRole.SupportPrimary -> ProgramUserRole.CoiRO,
    ProgramUserRole.SupportPrimary -> ProgramUserRole.Coi,
    ProgramUserRole.SupportPrimary -> ProgramUserRole.SupportSecondary,
    ProgramUserRole.SupportSecondary -> ProgramUserRole.CoiRO,
    ProgramUserRole.SupportSecondary -> ProgramUserRole.Coi,
    ProgramUserRole.SupportSecondary -> ProgramUserRole.SupportPrimary,
  ).foreach: (from, to) =>
    test(s"PI cannot change $from to $to"):
      val action = from match
        case ProgramUserRole.SupportPrimary | ProgramUserRole.SupportSecondary => "unassign"
        case _                                                                 => "assign"
      createProgramAs(pi1).flatMap: pid =>
        addProgramUserAs(staff, pid, from).flatMap: mid =>
          expect(
            user     = pi1,
            query    = changeUserRoleQuery(mid, to),
            expected = List(s"Only admin, staff or service users may $action support users.").asLeft
          )


  // What can a COI do?

  test("CoiRO can't change its own role"):
    createUsers(pi2) >>
    createProgramAs(pi1).flatMap: pid =>
      addProgramUserAs(pi1, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        linkUserAs(pi1, mid, pi2.id) >>
        expect(
          user     = pi2,
          query    = changeUserRoleQuery(mid, ProgramUserRole.Coi),
          expected = List(s"User ${pi2.id} is not authorized to perform this operation.").asLeft
        )

  // N.B. I'm not sure what the rule should be for downgrading CoiRo.  As
  // implemented it won't work though.  Would require a special case I think.
  test("Coi can downgrade itself".ignore):
    createUsers(pi2) >>
    createProgramAs(pi1).flatMap: pid =>
      addProgramUserAs(pi1, pid, ProgramUserRole.Coi).flatMap: mid =>
        linkUserAs(pi1, mid, pi2.id) >>
        expect(
          user     = pi2,
          query    = changeUserRoleQuery(mid, ProgramUserRole.CoiRO),
          expected = changeProgramUserRoleResult(ProgramUserRole.CoiRO).asRight
        )


  // What can an NGO user do?

  List(
    ProgramUserRole.CoiRO -> ProgramUserRole.Coi,
    ProgramUserRole.Coi   -> ProgramUserRole.CoiRO
  ).foreach: (from, to) =>
    test(s"Ngo (CA) can change $from to $to with allocated time."):
      for
        _   <- createUsers(pi1, pi2, admin, ngo)
        pid <- createProgramAs(pi1)
        _   <- setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, TimeSpan.Max) // so ngo can see the program
        mid <- addProgramUserAs(pi1, pid, from, partnerLinkFor(from))
        _   <- expect(
          user     = ngo,
          query    = changeUserRoleQuery(mid, to),
          expected = changeProgramUserRoleResult(to).asRight
        )
      yield ()

  List(
    ProgramUserRole.CoiRO -> ProgramUserRole.Coi,
    ProgramUserRole.Coi   -> ProgramUserRole.CoiRO
  ).foreach: (from, to) =>
    test(s"Ngo (CA) cannot change $from to $to without allocated time."):
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, admin, ngo)
          pid <- createProgramAs(pi1)
          mid <- addProgramUserAs(pi1, pid, from, partnerLinkFor(from))
          _   <- query(
            user     = ngo,
            query    = changeUserRoleQuery(mid, to)
          )
        yield ()
      } {
        case OdbError.NotAuthorized(ngo.id, _) => // expected
      }

  List(
    ProgramUserRole.CoiRO -> ProgramUserRole.SupportPrimary,
    ProgramUserRole.Coi   -> ProgramUserRole.SupportPrimary,
    ProgramUserRole.CoiRO -> ProgramUserRole.SupportSecondary,
    ProgramUserRole.Coi   -> ProgramUserRole.SupportSecondary,
    ProgramUserRole.SupportPrimary -> ProgramUserRole.CoiRO,
    ProgramUserRole.SupportPrimary -> ProgramUserRole.Coi,
    ProgramUserRole.SupportPrimary -> ProgramUserRole.SupportSecondary,
    ProgramUserRole.SupportSecondary -> ProgramUserRole.CoiRO,
    ProgramUserRole.SupportSecondary -> ProgramUserRole.Coi,
    ProgramUserRole.SupportSecondary -> ProgramUserRole.SupportPrimary,
  ).foreach: (from, to) =>
    test(s"Ngo (CA) cannot change $from to $to even with allocated time."):
      val action = from match
        case ProgramUserRole.SupportPrimary | ProgramUserRole.SupportSecondary => "unassign"
        case _                                                                 => "assign"
      for
        _   <- createUsers(pi1, pi2, admin, ngo)
        pid <- createProgramAs(pi1)
        _   <- setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, TimeSpan.Max) // so ngo can see the program
        mid <- addProgramUserAs(staff, pid, from, partnerLinkFor(from))
        _   <- expect(
          user     = ngo,
          query    = changeUserRoleQuery(mid, to),
          expected = List(s"Only admin, staff or service users may $action support users.").asLeft
        )
      yield ()

  // What can superusers do?

  List(staff, admin, service).foreach: u =>
    List(
      ProgramUserRole.CoiRO -> ProgramUserRole.Coi,
      ProgramUserRole.CoiRO -> ProgramUserRole.SupportPrimary,
      ProgramUserRole.CoiRO -> ProgramUserRole.SupportSecondary,
      ProgramUserRole.Coi   -> ProgramUserRole.CoiRO,
      ProgramUserRole.Coi   -> ProgramUserRole.SupportPrimary,
      ProgramUserRole.Coi   -> ProgramUserRole.SupportSecondary,
      ProgramUserRole.SupportPrimary -> ProgramUserRole.CoiRO,
      ProgramUserRole.SupportPrimary -> ProgramUserRole.Coi,
      ProgramUserRole.SupportPrimary -> ProgramUserRole.SupportSecondary,
      ProgramUserRole.SupportSecondary -> ProgramUserRole.CoiRO,
      ProgramUserRole.SupportSecondary -> ProgramUserRole.Coi,
      ProgramUserRole.SupportSecondary -> ProgramUserRole.SupportPrimary,
    ).foreach: (from, to) =>
      test(s"${u.role.access} can change $from to $to."):
        for
          _   <- createUsers(pi1, u)
          pid <- createProgramAs(pi1)
          _   <- setOneAllocationAs(u, pid, TimeAccountingCategory.CA, ScienceBand.Band1, TimeSpan.Max) // so ngo can see the program
          mid <- addProgramUserAs(u, pid, from, partnerLinkFor(from))
          _   <- expect(
            user     = u,
            query    = changeUserRoleQuery(mid, to),
            expected = changeProgramUserRoleResult(to).asRight
          )
        yield ()

  List(pi1, staff, admin, service).foreach: u =>
    test(s"${u.role.access} cannot add another PI."):
      interceptOdbError {
        for
          _   <- createUsers(pi1, admin)
          pid <- createProgramAs(pi1)
          mid <- addProgramUserAs(u, pid, ProgramUserRole.Coi, partnerLinkFor(ProgramUserRole.Coi))
          _   <- query(
            user    = u,
            query   = changeUserRoleQuery(mid, ProgramUserRole.Pi)
          )
        yield ()
      } {
        case OdbError.InvalidArgument(Some("Argument 'input' is invalid: PIs are added at program creation time.")) => // ok
      }