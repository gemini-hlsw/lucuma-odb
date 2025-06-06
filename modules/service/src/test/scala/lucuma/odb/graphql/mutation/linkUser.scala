// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.syntax.timespan.*
import lucuma.odb.data.OdbError

class linkUser extends OdbSuite:

  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, pi2, pi3, ngo, staff, admin, guest, service)

  // LINKING A COI

  test("[coi] guest user can't link a coi"):
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap: pid =>
      addProgramUserAs(staff, pid, ProgramUserRole.Coi).flatMap: mid =>
        interceptGraphQL(s"Guest users may not add CoIs."):
          linkUserAs(guest, mid, pi.id)

  test("[coi] pi user can link coi to program they own"):
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid, ProgramUserRole.Coi).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id)

  test("[coi] pi user can't link another coi to program where they are a coi"):
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid, ProgramUserRole.Coi).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation."):
          addProgramUserAs(staff, pid, ProgramUserRole.Coi).flatMap: rid2 =>
            linkUserAs(pi2, rid2, pi3.id)

  test("[coi] pi user can't link coi to program they don't own"):
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation."):
          linkUserAs(pi2, mid, pi3.id)

  test("[coi] service, admin, and staff users can add a coi to any program"):
    List(service, admin, staff).traverse_ : user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid).flatMap: mid =>
          linkUserAs(user, mid, pi2.id)

  test("[coi] ngo user can add coi to program with time allocated by user's partner"):
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, 42.hourTimeSpan) >>
        linkUserAs(ngo, mid, pi2.id)

  test("[coi] ngo user can't add coi to program without time allocated by user's partner"):
    createUsers(pi, pi2, ngo) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        interceptGraphQL(s"User ${ngo.id} is not authorized to perform this operation."):
          linkUserAs(ngo, mid, pi2.id)

  test(s"[general] a PI can't also be a COI"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.Coi).flatMap: rid0 =>
        interceptGraphQL(s"User ${pi.id} is already linked to program ${pid}."):
          linkUserAs(admin, rid0, pi.id)

  List(ProgramUserRole.Coi, ProgramUserRole.CoiRO).foreach: role =>
    test(s"[general] can't re-link a Coi as $role"):
      createUsers(pi, pi2) >>
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid, ProgramUserRole.Coi).flatMap: rid0 =>
          linkUserAs(pi, rid0, pi2.id) >>
          addProgramUserAs(pi, pid, role).flatMap: rid1 =>
            interceptGraphQL(s"User ${pi2.id} is already linked to program ${pid}."):
              linkUserAs(pi, rid1, pi2.id)

  // LINKING AN OBSERVER

  test("[observer] guest user can't link an observer"):
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap: pid =>
      addProgramUserAs(staff, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        interceptOdbError {
          linkUserAs(guest, mid, pi.id)
        } {
          case OdbError.NotAuthorized(guest.id, _) =>
        }

  test("[observer] pi user can link observer to program they own"):
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id)

  test("[observer] pi user can't link observer to program they don't own"):
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation."):
          linkUserAs(pi2, mid, pi3.id)

  test("[observer] pi user can link an observer to a program where they are a coi"):
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.Coi).flatMap: rid2 =>
        linkUserAs(pi, rid2, pi2.id) >>   // pi links pi2 as coi
        addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: rid3 =>
          linkUserAs(pi2, rid3, pi3.id)   // pi2 links pi3 as observer

  test("[observer] pi user can't link an observer to a program where they are an observer"):
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id)  >>   // pi links pi2 as observer
        addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: rid2 =>
          interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation."):
            linkUserAs(pi2, rid2, pi3.id) // pi2 tries to link pi3 as observer

  test("[observer] service, admin, and staff users can add an observer to any program"):
    List(service, admin, staff).traverse_ : user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: mid =>
          linkUserAs(user, mid, pi2.id)

  test("[observer] ngo user can add observer to program with time allocated by user's partner"):
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, 42.hourTimeSpan) >>
        linkUserAs(ngo, mid, pi2.id)

  test("[observer] ngo user can't add observer to program without time allocated by user's partner"):
    createUsers(pi, pi2, ngo) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        interceptGraphQL(s"User ${ngo.id} is not authorized to perform this operation."):
          linkUserAs(ngo, mid, pi2.id)

  // LINKING EXTERNAL USERS

  test("[external] guest user can't link an external user"):
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap: pid =>
      addProgramUserAs(staff, pid, ProgramUserRole.External).flatMap: mid =>
        interceptOdbError {
          linkUserAs(guest, mid, pi.id)
        } {
          case OdbError.NotAuthorized(guest.id, _) =>
        }

  test("[external] pi user can link external user to program they own"):
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.External).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id)

  test("[external] pi user can't link external user to program they don't own"):
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.External).flatMap: mid =>
        interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation."):
          linkUserAs(pi2, mid, pi3.id)

  test("[external] pi user can link an external user to a program where they are a coi"):
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.Coi).flatMap: rid2 =>
        linkUserAs(pi, rid2, pi2.id) >>   // pi links pi2 as coi
        addProgramUserAs(pi, pid, ProgramUserRole.External).flatMap: rid3 =>
          linkUserAs(pi2, rid3, pi3.id)   // pi2 links pi3 as external user

  test("[external] pi user can't link an external user to a program where they are an observer"):
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id)  >>   // pi links pi2 as observer
        addProgramUserAs(pi, pid, ProgramUserRole.External).flatMap: rid2 =>
          interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation."):
            linkUserAs(pi2, rid2, pi3.id) // pi2 tries to link pi3 as observer

  test("[external] service, admin, and staff users can add an external user to any program"):
    List(service, admin, staff).traverse_ : user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid, ProgramUserRole.External).flatMap: mid =>
          linkUserAs(user, mid, pi2.id)

  test("[external] ngo user can add external user to program with time allocated by user's partner"):
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.External).flatMap: mid =>
        setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, 42.hourTimeSpan) >>
        linkUserAs(ngo, mid, pi2.id)

  test("[external] ngo user can't add observer to program without time allocated by user's partner"):
    createUsers(pi, pi2, ngo) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.External).flatMap: mid =>
        interceptGraphQL(s"User ${ngo.id} is not authorized to perform this operation."):
          linkUserAs(ngo, mid, pi2.id)

  // LINKING SUPPORT

  List[ProgramUserRole.SupportPrimary.type | ProgramUserRole.SupportSecondary.type](
    ProgramUserRole.SupportPrimary,
    ProgramUserRole.SupportSecondary
  ).foreach: role =>

    test(s"[$role] guest user can't link a staff support user"):
      createUsers(guest, pi) >>
      createProgramAs(guest).flatMap: pid =>
        addProgramUserAs(staff, pid, role).flatMap: mid =>
          interceptGraphQL("Only admin, staff or service users may add support users."):
            linkUserAs(guest, mid, pi.id)

    test(s"[$role] pi user can't link a staff support user"):
      createUsers(pi, pi2) >>
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(staff, pid, role).flatMap: mid =>
          interceptGraphQL("Only admin, staff or service users may add support users."):
            linkUserAs(pi, mid, pi2.id)

    test(s"[$role] service, admin, and staff users can add a staff support user to any program"):
      List(service, admin, staff).traverse_ : user =>
        createUsers(user) >>
        createProgramAs(pi).flatMap: pid =>
          addProgramUserAs(staff, pid, role).flatMap: mid =>
            linkUserAs(user, mid, pi2.id)

    test(s"[$role] ngo user can't add staff support to program with time allocated by user's partner"):
      createUsers(pi, pi2, ngo, admin) >>
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(staff, pid, role).flatMap: mid =>
          setOneAllocationAs(admin, pid, TimeAccountingCategory.CA, ScienceBand.Band1, 42.hourTimeSpan) >>
          interceptGraphQL("Only admin, staff or service users may add support users."):
            linkUserAs(ngo, mid, pi2.id)

    test(s"[$role] pi can also be a support user"):
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(staff, pid, role).flatMap: mid =>
          linkUserAs(staff, mid, pi.id)

    test(s"[$role] coi can also be a support user"):
      createUsers(pi2, admin) >>
      createProgramAs(pi).flatMap: pid =>
        addProgramUserAs(pi, pid, ProgramUserRole.Coi).flatMap: rid0 =>
          linkUserAs(pi, rid0, pi2.id) >>
          addProgramUserAs(staff, pid, role).flatMap: rid1 =>
            linkUserAs(staff, rid1, pi2.id)

  test(s"[${ProgramUserRole.SupportPrimary}] same user can be both primary and secondary"):
    createUsers(pi2, admin) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid, ProgramUserRole.SupportPrimary).flatMap: rid0 =>
        linkUserAs(admin, rid0, pi2.id) >>
        addProgramUserAs(staff, pid, ProgramUserRole.SupportSecondary).flatMap: rid1 =>
          linkUserAs(admin, rid1, pi2.id)


  // GENERAL RULES

  test("[general] can't link a guest user"):
    createUsers(pi, guest) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(staff, pid, ProgramUserRole.Coi).flatMap: mid =>
        interceptGraphQL(s"User ${guest.id} does not exist or is of a nonstandard type."):
          linkUserAs(pi, mid, guest.id)

  test("[general] can't link a PI user"):
    createUsers(pi) >>
    createProgramAs(pi).flatMap: pid =>
      interceptGraphQL(s"Argument 'input' is invalid: PIs are added at program creation time."):
        addProgramUserAs(staff, pid, ProgramUserRole.Pi)

  test("[general] can't link a service user"):
    createUsers(pi, service) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        interceptGraphQL(s"User ${service.id} does not exist or is of a nonstandard type."):
          linkUserAs(pi, mid, service.id)
