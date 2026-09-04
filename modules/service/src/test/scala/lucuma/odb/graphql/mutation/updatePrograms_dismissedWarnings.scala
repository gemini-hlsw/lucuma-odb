// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.literal.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError

class updatePrograms_dismissedWarnings extends OdbSuite:

  val guest    = TestUsers.guest(5)
  val pi       = TestUsers.Standard.pi(1, 101)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, staff, admin, guest, service).toList

  def updatedismissedWarningsQuery(pid: Program.Id, codes: ObservationValidationCode*): String = 
    s"""
      mutation {
        updatePrograms(
          input: {
            SET: {
              dismissedWarnings: ${codes.map(_.tag.toScreamingSnakeCase).mkString("[", " ", "]")}
            }
            WHERE: {
              id: {
                EQ: "$pid"
              }
            }
          }
        ) {
          hasMore
          programs {
            id
            dismissedWarnings
          }
        }
      }
    """

  test(s"can't dismiss an error"):
    createProgramAs(pi).flatMap: pid =>
      expectOdbError(
        user = service,
        query = updatedismissedWarningsQuery(pid, ObservationValidationCode.Error.ItcError),
        expected = {
          case OdbError.InvalidArgument(Some("Argument 'input.SET.dismissedWarnings' is invalid: at index 0: Fatal error ITC_ERROR cannot be dismissed.")) => () // expected
        }
      )

  List(guest, pi).foreach: u =>
    test(s"${u.role.access} can't update dismissed validation set"):
      createProgramAs(u).flatMap: pid =>
        expectOdbError(
          user = u,
          query = updatedismissedWarningsQuery(pid, ObservationValidationCode.Warning.ConditionsUnlikely),
          expected = {
            case OdbError.NotAuthorized(u, Some("Only staff may set the dismissed validations.")) => () // expected
          }
        )

  List(staff, admin, service).foreach: u =>
    test(s"${u.role.access} can update dismissed validation set"):
      createProgramAs(u).flatMap: pid =>
        expect(
          user = u,
          query = updatedismissedWarningsQuery(pid, ObservationValidationCode.Warning.ConditionsUnlikely, ObservationValidationCode.Warning.LowTotalSignalToNoise),
          expected = Right(
            json"""
              {
                "updatePrograms": {
                  "hasMore": false,
                  "programs": [
                    {
                      "id": $pid,
                      "dismissedWarnings": [
                        "CONDITIONS_UNLIKELY",
                        "LOW_TOTAL_SIGNAL_TO_NOISE"
                      ]                  
                    }
                  ]
                }
              }
            """
          )
        )

