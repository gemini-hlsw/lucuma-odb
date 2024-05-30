// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.data.ProgramUserRole

class unlinkUser extends OdbSuite {

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
    uid:  User.Id,
    pid:  Program.Id,
  ): IO[Boolean] =
    query(
      user = user,
      query = s"""
        mutation {
          unlinkUser(input: {
            programId: ${pid.asJson}
            userId: ${uid.asJson}
          }) {
            result
          }
        }
      """
    ).map: j =>
      j.hcursor.downFields("unlinkUser", "result").require[Boolean]

  // General rules

  test("Unlink should return false if link doesn't exist.") {
    for
      _   <- createUsers(pi1, pi2)
      pid <- createProgramAs(pi1)
      _   <- assertIO(unlinkAs(pi1, pi2.id, pid), false)
    yield ()
  }

  test("Unlink should return false if program isn't visible.") {
    for
      _   <- createUsers(pi1, pi2, pi3)
      pid <- createProgramAs(pi1)
      _   <- linkCoiAs(pi1, pi2.id -> pid, Partner.US)
      _   <- assertIO(unlinkAs(pi3, pi2.id, pid), false)
    yield ()
  }

  // What can a Guest do?

  Enumerated[ProgramUserRole].all.foreach: role =>
    test(s"Guest can't unlink $role (NotAuthorized).") {
      interceptOdbError {
        for
          _   <- createUsers(guest, pi2, admin)
          pid <- createProgramAs(guest)
          _   <- linkAs(admin, pi2.id, pid, role, Option.when(role != ProgramUserRole.Support)(Partner.US))
          _   <- unlinkAs(guest, pi2.id, pid)
        yield ()
      } {
        case OdbError.NotAuthorized(guest.id, _) => // this is what we expect
      }
    }

  // What can a PI do?

  List(ProgramUserRole.CoiRO, ProgramUserRole.Coi).foreach: link =>
    test(s"PI can unlink $link") {
      for
        _   <- createUsers(pi1, pi2, pi3)
        pid <- createProgramAs(pi1)
        _   <- linkAs(pi1, pi2.id, pid, link, Some(Partner.CA))
        _   <- assertIO(unlinkAs(pi1, pi2.id, pid), true)
      yield ()
    }

  List(ProgramUserRole.Support).foreach: link =>
    test(s"PI can't unlink $link (NotAuthorized).") {
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, pi3, admin)
          pid <- createProgramAs(pi1)
          _   <- linkAs(admin, pi2.id, pid, link, Option.when(link != ProgramUserRole.Support)(Partner.US))
          _   <- unlinkAs(pi1, pi2.id, pid)
        yield ()
      } {
        case OdbError.NotAuthorized(pi1, _) => // this is what we expect
      }
    }

  // What can a Coi do?

  test("Coi can unlink an observer") {
    for
      _   <- createUsers(pi1, pi2, pi3)
      pid <- createProgramAs(pi1)
      _   <- linkCoiAs(pi1, pi2.id -> pid, Partner.AR)
      _   <- linkObserverAs(pi1, pi3.id -> pid, Partner.CA)
      _   <- unlinkAs(pi2, pi3.id, pid)
    yield ()
  }

  List(ProgramUserRole.Coi, ProgramUserRole.Support).foreach: link =>
    test(s"Coi can't unlink $link (NotAuthorized).") {
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, pi3, admin)
          pid <- createProgramAs(pi1)
          _   <- linkAs(admin, pi2.id, pid, link, Option.when(link != ProgramUserRole.Support)(Partner.US))
          _   <- linkCoiAs(pi1, pi3.id -> pid, Partner.US)
          _   <- unlinkAs(pi3, pi2.id, pid)
        yield ()
      } {
        case OdbError.NotAuthorized(pi1, _) => // this is what we expect
      }
    }

  // What can NGO user do?

  List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.Support).foreach: link =>
    test(s"Ngo (CA) can't unlink $link (NotAuthorized).") {
      interceptOdbError {
        for
          _   <- createUsers(pi1, pi2, admin, ngo)
          pid <- createProgramAs(pi1)
          _   <- linkAs(admin, pi2.id, pid, link, Option.when(link != ProgramUserRole.Support)(Partner.US))
          _   <- setAllocationAs(admin, pid, Partner.CA, TimeSpan.Max) // so ngo can see the program
          _   <- unlinkAs(ngo, pi2.id, pid)
        yield ()
      } {
        case OdbError.NotAuthorized(ngo, _) => // this is what we expect
      }
    }

  // What can superusers do?

  List(staff, admin, service).foreach: u =>
    List(ProgramUserRole.CoiRO, ProgramUserRole.Coi, ProgramUserRole.Support).foreach: link =>
      test(s"${u.role.access} can unlink $link.") {
        for
          _   <- createUsers(pi1, pi2, u)
          pid <- createProgramAs(pi1)
          _   <- linkAs(admin, pi2.id, pid, link, Option.when(link != ProgramUserRole.Support)(Partner.US))
          _   <- assertIO(unlinkAs(u, pi2.id, pid), true)
        yield ()
      }

}
