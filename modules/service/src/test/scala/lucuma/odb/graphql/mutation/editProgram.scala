package lucuma.odb.graphql
package mutation

import io.circe.literal._
import lucuma.odb.graphql.OdbSuite
import cats.syntax.all._

class editProgram extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)

  val validUsers = List(pi, guest, service).toList

  test("edits are applied".ignore) {
    // create a program
    // edit it, ensure edits are applied
  }

  test("empty edit is an error".ignore) {
    // create a program
    // ensure that an empty edit fails
  }

  test("Guest/Pi can edit program with Pi/Coi") {
  }


  test("Staff/Admin/Service can edit any program") {
  }

}