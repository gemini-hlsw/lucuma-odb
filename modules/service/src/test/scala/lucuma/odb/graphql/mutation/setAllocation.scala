// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import lucuma.core.model.Partner
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Tag

class setAllocation extends OdbSuite {

  val guest    = TestUsers.guest(nextId)
  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.Ca)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, ngo, staff, admin, guest, service)

  test("guest, pi, ngo can't set allocation") {
    List(guest, pi, ngo).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        interceptGraphQL(s"User ${user.id} is not authorized to perform this action") {
          setAllocationAs(user, pid, Tag("CA"), 42.hourTimeSpan)
        }
      }
    }
  }

  test("admin, staff, service can set (and update) allocation in any program") {
    createProgramAs(pi).flatMap { pid =>
      List((admin, 2L), (staff, 3L), (service, 4L)).traverse { case (user, hours) =>
        setAllocationAs(user, pid, Tag("US"), TimeSpan.fromHours(hours).get)
      }
    }
  }

}
