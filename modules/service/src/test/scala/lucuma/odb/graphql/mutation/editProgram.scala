package lucuma.odb.graphql
package mutation

import lucuma.odb.graphql.OdbSuite
import lucuma.core.model.Partner

class editProgram extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.Ca)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service).toList

  test("edits are applied".fail) {
    // create a program
    // edit it, ensure edits are applied
  }

  test("empty edit is an error".fail) {
    // create a program
    // ensure that an empty edit fails
  }

  test("Guest + Standard/pi can edit if they have Pi/Coi roles".fail) {
  }

  test("Guest + Standard/pi cannot edit without a role, nor with Observer or Support role".fail) {
  }

  test("Guest + Standard/ngo (non-admin) can edit with matching partner support role.".fail) {
  }

  test("Guest + Standard/ngo (non-admin) cannot edit a program with a matching time allocation.".fail) {
  }

  test("Guest + Standard/ngo (non-admin) cannot edit without a role, nor with coi, observer, non-matching partner support, or staff support role.".fail) {
  }

  test("Guest + Standard/ngo (admin) can edit with matching partner support role.".fail) {
  }

  test("Guest + Standard/ngo (admin) can edit a program with a matching time allocation.".fail) {
  }

  test("Guest + Standard/ngo (admin) cannot edit without a role, nor with coi, observer, non-matching partner support, or staff support role.".fail) {
  }

  test("Service + Standard/staff,admin users can edit regardless of roles".fail) {
  }

}