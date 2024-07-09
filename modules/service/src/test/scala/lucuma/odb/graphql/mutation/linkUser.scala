// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.Partner
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.data.ScienceBand

class linkUser extends OdbSuite {

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

  test("[coi] guest user can't link a coi") {
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap { pid =>
      interceptGraphQL(s"User ${guest.id} is not authorized to perform this operation.") {
        linkCoiAs(guest, pi.id -> pid, Partner.US)
      }
    }
  }

  test("[coi] pi user can link coi to program they own") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      linkCoiAs(pi, pi2.id -> pid, Partner.US)
    }
  }

  test("[coi] pi user can't link another coi to program where they are a coi") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      linkCoiAs(pi, pi2.id -> pid, Partner.US) >>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation.") {
        linkCoiAs(pi2, pi3.id -> pid, Partner.US)
      }
    }
  }

  test("[coi] pi user can't link coi to program they don't own") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation.") {
        linkCoiAs(pi2, pi3.id -> pid, Partner.US)
      }
    }
  }

  test("[coi] service, admin, and staff users can add a coi to any program") {
    List(service, admin, staff).traverse_ { user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap { pid =>
        linkCoiAs(user, pi2.id -> pid, Partner.US)
      }
    }
  }

  test("[coi] ngo user can add coi to program with time allocated by user's partner") {
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap { pid =>
      setOneAllocationAs(admin, pid, Partner.CA, ScienceBand.Band1, 42.hourTimeSpan) >>
      linkCoiAs(ngo, pi2.id -> pid, Partner.US)
    }
  }

  test("[coi] ngo user can't add coi to program without time allocated by user's partner") {
    createUsers(pi, pi2, ngo) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${ngo.id} is not authorized to perform this operation.") {
        linkCoiAs(ngo, pi2.id -> pid, Partner.US)
      }
    }
  }

  // LINKING AN OBSERVER

  test("[observer] guest user can't link an observer") {
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap { pid =>
      interceptOdbError {
        linkObserverAs(guest, pi.id -> pid, Partner.US)
      } {
        case OdbError.NotAuthorized(guest.id, _) =>
      }
    }
  }

  test("[observer] pi user can link observer to program they own") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      linkObserverAs(pi, pi2.id -> pid, Partner.US)
    }
  }

  test("[observer] pi user can't link observer to program they don't own") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation.") {
        linkObserverAs(pi2, pi3.id -> pid, Partner.US)
      }
    }
  }

  test("[observer] pi user can link an observer to a program where they are a coi") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      linkCoiAs(pi, pi2.id -> pid, Partner.US) >>     // pi links pi2 as coi
      linkObserverAs(pi2, pi3.id -> pid, Partner.US)  // pi2 links pi3 as observer
    }
  }

  test("[observer] pi user can't link an observer to a program where they are an observer") {
    createUsers(pi, pi2, pi3) >>
    createProgramAs(pi).flatMap { pid =>
      linkObserverAs(pi, pi2.id -> pid, Partner.US) >>  // pi links pi2 as observer
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation.") {
        linkObserverAs(pi2, pi3.id -> pid, Partner.US)   // pi2 tries to link pi3 as observer
      }
    }
  }

  test("[observer] service, admin, and staff users can add an observer to any program") {
    List(service, admin, staff).traverse_ { user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap { pid =>
        linkObserverAs(user, pi2.id -> pid, Partner.US)
      }
    }
  }

  test("[observer] ngo user can add observer to program with time allocated by user's partner") {
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap { pid =>
      setOneAllocationAs(admin, pid, Partner.CA, ScienceBand.Band1, 42.hourTimeSpan) >>
      linkObserverAs(ngo, pi2.id -> pid, Partner.US)
    }
  }

  test("[observer] ngo user can't add observer to program without time allocated by user's partner") {
    createUsers(pi, pi2, ngo) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${ngo.id} is not authorized to perform this operation.") {
        linkObserverAs(ngo, pi2.id -> pid, Partner.US)
      }
    }
  }

  // LINKING SUPPORT

  test("[staff support] guest user can't link a staff support user") {
    createUsers(guest, pi) >>
    createProgramAs(guest).flatMap { pid =>
      interceptGraphQL(s"User ${guest.id} is not authorized to perform this operation.") {
        linkSupportAs(guest, pi.id -> pid)
      }
    }
  }

  test("[staff support] pi user can't link a staff support user") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi.id} is not authorized to perform this operation.") {
        linkSupportAs(pi, pi2.id -> pid)
      }
    }
  }

  test("[staff support] service, admin, and staff users can add a staff support user to any program") {
    List(service, admin, staff).traverse_ { user =>
      createUsers(user) >>
      createProgramAs(pi).flatMap { pid =>
        linkSupportAs(user, pi2.id -> pid)
      }
    }
  }

  test("[staff support] ngo user can't add staff support to program with time allocated by user's partner") {
    createUsers(pi, pi2, ngo, admin) >>
    createProgramAs(pi).flatMap { pid =>
      setOneAllocationAs(admin, pid, Partner.CA, ScienceBand.Band1, 42.hourTimeSpan) >>
      interceptGraphQL(s"User ${ngo.id} is not authorized to perform this operation.") {
        linkSupportAs(ngo, pi2.id -> pid)
      }
    }
  }

  // GENERAL RULES

  test("[general] can't re-link a user") {
    createUsers(pi, pi2) >>
    createProgramAs(pi).flatMap { pid =>
      linkCoiAs(pi, pi2.id -> pid, Partner.US) >>
      interceptGraphQL(s"User ${pi2.id} is already linked to program ${pid}.") {
        linkCoiAs(pi, pi2.id -> pid, Partner.US)
      }
    }
  }

  test("[general] can't link a guest user") {
    createUsers(pi, guest) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${guest.id} does not exist or is of a nonstandard type.") {
        linkCoiAs(pi, guest.id -> pid, Partner.US)
      }
    }
  }

  test("[general] can't link a service user") {
    createUsers(pi, service) >>
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${service.id} does not exist or is of a nonstandard type.") {
        linkCoiAs(pi, service.id -> pid, Partner.US)
      }
    }
  }

}
