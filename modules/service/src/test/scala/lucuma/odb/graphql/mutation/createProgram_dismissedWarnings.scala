// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.model.User
import lucuma.odb.data.OdbError

class createProgram_dismissedWarnings extends OdbSuite:

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.CA)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service)

  val CreateWithdismissedWarnings =
    """
      mutation {
        createProgram(
          input: {
            SET: {
              name: "Foo",
              dismissedWarnings: [
                GENERIC_WARNING
                LOW_TOTAL_SIGNAL_TO_NOISE
              ]
            }
          }
        ) {
          program {
            dismissedWarnings
          }
        }
      }
    """

  List(guest, pi).foreach: u =>
    test(s"${u.role.access} can't initialize dismissed validation set"):
      expectOdbError(
        user     = pi,
        query    = CreateWithdismissedWarnings,
        expected = {
          case OdbError.NotAuthorized(u, Some("Only staff may set the dismissed validations.")) => () // expected
        }
      )
  
  List(staff, admin, service).foreach: u =>
    test(s"${u.role.access} can initialize dismissed validation set"):
      expect(
        user     = staff,
        query    = CreateWithdismissedWarnings,
        expected = Right(
          json"""
            {
              "createProgram": {
                "program": {
                  "dismissedWarnings": [
                    "GENERIC_WARNING",
                    "LOW_TOTAL_SIGNAL_TO_NOISE"
                  ]                  
                }
              }
            }
          """
        )
      )
