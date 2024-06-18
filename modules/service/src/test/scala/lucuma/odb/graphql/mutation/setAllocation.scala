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
import io.circe.literal.*

class setAllocation extends OdbSuite {

  val guest    = TestUsers.guest(nextId)
  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, ngo, staff, admin, guest, service)

  test("guest, pi, ngo can't set allocation") {
    List(guest, pi, ngo).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        interceptGraphQL(s"User ${user.id} is not authorized to perform this operation.") {
          setAllocationAs(user, pid, Partner.CA, 42.hourTimeSpan)
        }
      }
    }
  }

  test("admin, staff, service can set (and update) allocation in any program") {
    createProgramAs(pi).flatMap { pid =>
      List((admin, 2L), (staff, 3L), (service, 4L)).traverse { case (user, hours) =>
        setAllocationAs(user, pid, Partner.US, TimeSpan.fromHours(hours).get)
      }
    }
  }

  test("should be able to read allocations back") {
    createProgramAs(pi).flatMap { pid =>
      setAllocationAs(staff, pid, Partner.US, TimeSpan.fromHours(1.23).get) >>
      setAllocationAs(staff, pid, Partner.CA, TimeSpan.fromHours(4.56).get) >>
      expect(
        user = pi,
        query = s"""
          query {
            program(programId: "$pid") {
              allocations {
                partner
                duration {
                  hours
                }              
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "program" : {
                "allocations" : [
                  {
                    "partner" : "CA",
                    "duration" : {
                      "hours" : 4.560000
                    }
                  },
                  {
                    "partner" : "US",
                    "duration" : {
                      "hours" : 1.230000
                    }
                  }
                ]
              }
            }
          """
        )
      )
    }
  }

}
