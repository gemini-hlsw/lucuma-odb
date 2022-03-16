package lucuma.odb.graphql
package mutation

import lucuma.odb.graphql.OdbSuite
import lucuma.core.model.Partner
import cats.effect.IO
import cats.syntax.all._
import lucuma.core.model.User
import lucuma.core.model.Program
import io.circe.literal._
import clue.ResponseException
import lucuma.odb.data.ProgramUserRole
import io.circe.syntax._
import lucuma.odb.data.ProgramUserSupportType

class linkUser extends OdbSuite {

  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.Ca)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, pi2, pi3, ngo, staff, admin, guest, service)

  def createProgramAs(user: User): IO[Program.Id] =
    query(user, "mutation { createProgram(input: { name: null }) { id } }").flatMap { js =>
      js.hcursor
        .downField("createProgram")
        .downField("id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

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
            role
            userId
          }
        }
      """,
      expected = json"""
        {
          "linkUser" : {
            "role" : $role,
            "userId" : $uid
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

  def createUsers(users: User*): IO[Unit] =
    users.toList.traverse_(createProgramAs) // TODO: something cheaper

  /** Ensure that exactly the specified errors are reported, in order. */
  def interceptGraphQL(messages: String*)(fa: IO[_]): IO[Unit] =
    fa.attempt.flatMap {
      case Left(e: ResponseException) =>
        assertEquals(messages.toList, e.asGraphQLErrors.toList.flatten.map(_.message)).pure[IO]
      case Left(other) => IO.raiseError(other)
      case Right(a) => fail(s"Expected failure, got $a")
    }


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
    // TODO
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

  test("[coi] ngo user can add observer to program with time allocated by user's partner".ignore) {
    // TODO
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

  test("[staff support] ngo user can't link a staff support user".ignore) {
  }

  // LINKING NGO SUPPORT

  test("[ngo support] guest user can't link a ngo support user".ignore) {
  }

  test("[ngo support] pi user can't link a ngo support user".ignore) {
  }

  test("[ngo support] service, admin, and service users can add a ngo support user to any program".ignore) {
  }

  test("[ngo support] ngo user can't link a ngo support user".ignore) {
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