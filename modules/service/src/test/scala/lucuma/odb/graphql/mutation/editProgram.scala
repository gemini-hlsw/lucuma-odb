// package lucuma.odb.graphql
// package mutation

// import lucuma.odb.graphql.OdbSuite
// import lucuma.core.model.Partner

// class editProgram extends OdbSuite {

//   val pi       = TestUsers.Standard.pi(1, 101)
//   val ngo      = TestUsers.Standard.ngo(2, 102, Partner.Ca)
//   val staff    = TestUsers.Standard.staff(3, 103)
//   val admin    = TestUsers.Standard.admin(4, 104)
//   val guest    = TestUsers.guest(5)
//   val service  = TestUsers.service(6)

//   val validUsers = List(pi, ngo, staff, admin, guest, service).toList

//   test("edits are applied".ignore) {
//   }

//   test("empty edit is an error".ignore) {
//   }

//   test("Guest + Standard/pi can edit if they have Pi/Coi roles".ignore) {
//   }

//   test("Guest + Standard/pi cannot edit without a role, nor with Observer or Support role".ignore) {
//   }

//   test("Guest + Standard/ngo (non-admin) can edit with matching partner support role.".ignore) {
//   }

//   test("Guest + Standard/ngo (non-admin) cannot edit a program with a matching time allocation.".ignore) {
//   }

//   test("Guest + Standard/ngo (non-admin) cannot edit without a role, nor with coi, observer, non-matching partner support, or staff support role.".ignore) {
//   }

//   test("Guest + Standard/ngo (admin) can edit with matching partner support role.".ignore) {
//   }

//   test("Guest + Standard/ngo (admin) can edit a program with a matching time allocation.".ignore) {
//   }

//   test("Guest + Standard/ngo (admin) cannot edit without a role, nor with coi, observer, non-matching partner support, or staff support role.".ignore) {
//   }

//   test("Service + Standard/staff,admin users can edit regardless of roles".ignore) {
//   }

// }