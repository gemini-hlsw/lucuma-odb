// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import lucuma.odb.graphql.OdbSuite

import java.time.Duration

class linkUser extends OdbSuite with CreateProgramOps with LinkUserOps with SetAllocationOps {

  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.Ca)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, pi2, pi3, ngo, staff, admin, guest, service)

  def createUsers(users: User*): IO[Unit] =
    users.toList.traverse_(createProgramAs) // TODO: something cheaper

  // LINKING A COI

  test("[coi] guest user can't link a coi") {
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap { pid =>
      interceptGraphQL(s"User ${guest.id} is not authorized to perform this action") {
        linkCoiAs(guest, pi.id -> pid)
      }
    }
  }

  test("[coi] pi user can link coi to program they own") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      linkCoiAs(pi, pi2.id -> pid)
    }
  }

  test("[coi] pi user can't link another coi to program where they are a coi") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      linkCoiAs(pi, pi2.id -> pid) >>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this action") {
        linkCoiAs(pi2, pi3.id -> pid)
      }
    }
  }

  test("[coi] pi user can't link coi to program they don't own") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this action") {
        linkCoiAs(pi2, pi3.id -> pid)
      }
    }
  }

  test("[coi] service, admin, and staff users can add a coi to any program") {
    List(service, admin, staff).traverse_ { user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap { pid =>
        linkCoiAs(user, pi2.id -> pid)
      }
    }
  }

  test("[coi] ngo user can add coi to program with time allocated by user's partner".ignore) {
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap { pid =>
      setAllocationAs(admin, pid, Tag("ca"), Duration.ofHours(42)) >>
      linkCoiAs(ngo, pi2.id -> pid)
    }
  }

  test("[coi] ngo user can't add coi to program without time allocated by user's partner") {
    createUsers(pi, pi2, ngo) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${ngo.id} is not authorized to perform this action") {
        linkCoiAs(ngo, pi2.id -> pid)
      }
    }
  }

  // LINKING AN OBSERVER

  test("[observer] guest user can't link an observer") {
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap { pid =>
      interceptGraphQL(s"User ${guest.id} is not authorized to perform this action") {
        linkObserverAs(guest, pi.id -> pid)
      }
    }
  }

  test("[observer] pi user can link observer to program they own") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      linkObserverAs(pi, pi2.id -> pid)
    }
  }

  test("[observer] pi user can't link observer to program they don't own") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this action") {
        linkObserverAs(pi2, pi3.id -> pid)
      }
    }
  }

  test("[observer] pi user can link an observer to a program where they are a coi") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      linkCoiAs(pi, pi2.id -> pid) >>     // pi links pi2 as coi
      linkObserverAs(pi2, pi3.id -> pid)  // pi2 links pi3 as observer
    }
  }

  test("[observer] pi user can't link an observer to a program where they are an observer") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      linkObserverAs(pi, pi2.id -> pid) >>  // pi links pi2 as observer
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this action") {
        linkObserverAs(pi2, pi3.id -> pid)   // pi2 tries to link pi3 as observer
      }
    }
  }

  test("[observer] service, admin, and staff users can add an observer to any program") {
    List(service, admin, staff).traverse_ { user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap { pid =>
        linkObserverAs(user, pi2.id -> pid)
      }
    }
  }

  test("[observer] ngo user can add observer to program with time allocated by user's partner".ignore) {
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap { pid =>
      setAllocationAs(admin, pid, Tag("ca"), Duration.ofHours(42)) >>
      linkObserverAs(ngo, pi2.id -> pid)
    }
  }

  test("[observer] ngo user can't add observer to program without time allocated by user's partner") {
    createUsers(pi, pi2, ngo) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${ngo.id} is not authorized to perform this action") {
        linkObserverAs(ngo, pi2.id -> pid)
      }
    }
  }

  // LINKING STAFF SUPPORT

  test("[staff support] guest user can't link a staff support user") {
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap { pid =>
      interceptGraphQL(s"User ${guest.id} is not authorized to perform this action") {
        linkStaffSupportAs(guest, pi.id -> pid)
      }
    }
  }

  test("[staff support] pi user can't link a staff support user") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi.id} is not authorized to perform this action") {
        linkStaffSupportAs(pi, pi2.id -> pid)
      }
    }
  }

  test("[staff support] service, admin, and staff users can add a staff support user to any program") {
    List(service, admin, staff).traverse_ { user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap { pid =>
        linkStaffSupportAs(user, pi2.id -> pid)
      }
    }
  }

  test("[staff support] ngo user can't add staff support to program with time allocated by user's partner".ignore) {
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap { pid =>
      setAllocationAs(admin, pid, Tag("ca"), Duration.ofHours(42)) >>
      interceptGraphQL(s"User ${ngo.id} is not authorized to perform this action") {
        linkStaffSupportAs(ngo, pi2.id -> pid)
      }
    }
  }

  // LINKING NGO SUPPORT

  test("[ngo support] guest user can't link a ngo support user") {
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap { pid =>
      interceptGraphQL(s"User ${guest.id} is not authorized to perform this action") {
        linkNgoSupportAs(guest, pi.id -> pid, Partner.Br)
      }
    }
  }

  test("[ngo support] pi user can't link a ngo support user") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi.id} is not authorized to perform this action") {
        linkNgoSupportAs(pi, pi2.id -> pid, Partner.Br)
      }
    }
  }

  test("[ngo support] staff, admin, and service users can add a ngo support user to any program") {
    createUsers(pi, pi2) >>
    List(staff, service, admin).traverse { u =>
      createProgramAs(pi).flatMap { pid =>
        linkNgoSupportAs(u, pi2.id -> pid, Partner.Br)
      }
    }
  }

  test("[ngo support] ngo user can't link a ngo support user") {
    createUsers(ngo, pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${ngo.id} is not authorized to perform this action") {
        linkNgoSupportAs(ngo, pi2.id -> pid, Partner.Br)
      }
    }
  }

  // GENERAL RULES

  test("[general] can't re-link a user") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      linkCoiAs(pi, pi2.id -> pid) >>
      interceptGraphQL(s"User ${pi2.id} is already linked to program ${pid}.") {
        linkCoiAs(pi, pi2.id -> pid)
      }
    }
  }

  test("[general] can't link a guest user") {
    createUsers(pi, guest) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${guest.id} does not exist or is of a nonstandard type.") {
        linkCoiAs(pi, guest.id -> pid)
      }
    }
  }

  test("[general] can't link a service user") {
    createUsers(pi, service) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${service.id} does not exist or is of a nonstandard type.") {
        linkCoiAs(pi, service.id -> pid)
      }
    }
  }

}

trait LinkUserOps { this: OdbSuite =>

  def linkAs(
    user: User,
    uid: User.Id,
    pid: Program.Id,
    role: ProgramUserRole,
    supportType: Option[ProgramUserSupportType] = None,
    partner: Option[Partner] = None,
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          linkUser(input: {
            programId: ${pid.asJson}
            userId: ${uid.asJson}
            role: ${role.tag.toUpperCase}
            supportType: ${supportType.fold("null")(_.tag.toUpperCase)}
            supportPartner: ${partner.fold("null")(_.tag.toUpperCase)}
          }) {
            user {
              role
              userId
            }
          }
        }
      """,
      expected = json"""
        {
          "linkUser" : {
            "user": {
              "role" : $role,
              "userId" : $uid
            }
          }
        }
      """.asRight
    )

  def linkCoiAs(user: User, uid: User.Id, pid: Program.Id): IO[Unit] =
    linkAs(user, uid, pid, ProgramUserRole.Coi)

  def linkCoiAs(user: User, arrow: (User.Id, Program.Id)): IO[Unit] =
    linkCoiAs(user, arrow._1, arrow._2)

  def linkObserverAs(user: User, uid: User.Id, pid: Program.Id): IO[Unit] =
    linkAs(user, uid, pid, ProgramUserRole.Observer)

  def linkObserverAs(user: User, arrow: (User.Id, Program.Id)): IO[Unit] =
    linkObserverAs(user, arrow._1, arrow._2)

  def linkStaffSupportAs(user: User, uid: User.Id, pid: Program.Id): IO[Unit] =
    linkAs(user, uid, pid, ProgramUserRole.Support, Some(ProgramUserSupportType.Staff))

  def linkStaffSupportAs(user: User, arrow: (User.Id, Program.Id)): IO[Unit] =
    linkStaffSupportAs(user, arrow._1, arrow._2)

  def linkNgoSupportAs(user: User, uid: User.Id, pid: Program.Id, partner: Partner): IO[Unit] =
    linkAs(user, uid, pid, ProgramUserRole.Support, Some(ProgramUserSupportType.Partner), Some(partner))

  def linkNgoSupportAs(user: User, arrow: (User.Id, Program.Id), partner: Partner): IO[Unit] =
    linkNgoSupportAs(user, arrow._1, arrow._2, partner)

}