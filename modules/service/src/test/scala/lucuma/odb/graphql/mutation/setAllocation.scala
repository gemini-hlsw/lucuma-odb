package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.graphql.OdbSuite
import java.time.Duration

class setAllocation extends OdbSuite with CreateProgramOps with SetAllocationOps {

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
          setAllocationAs(user, pid, Tag("CA"), Duration.ofHours(42))
        }
      }
    }
  }

  test("admin, staff, service can set (and update) allocation in any program") {
    createProgramAs(pi).flatMap { pid =>
      List((admin, 2L), (staff, 3L), (service, 4L)).traverse { case (user, hours) =>
        setAllocationAs(user, pid, Tag("US"), Duration.ofHours(hours))
      }
    }
  }

}

trait SetAllocationOps { this: OdbSuite =>

  def setAllocationAs(
    user: User,
    pid: Program.Id,
    partner: Tag,
    duration: Duration,
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          setAllocation(input: {
            programId: ${pid.asJson}
            partner:   ${partner.value.toUpperCase}
            duration:  ${duration.asJson}
          }) {
            partner
            duration
          }
        }
      """,
      expected = json"""
        {
          "setAllocation" : {
            "partner":  ${partner.asJson},
            "duration": ${duration.asJson}
          }
        }
      """.asRight
    )

}