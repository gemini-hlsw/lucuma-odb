// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class updateConfigurationRequests extends OdbSuite with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(3, 34)
  val admin   = TestUsers.Standard.admin(2, 32)

  val validUsers = List(pi, pi2, admin).toList

  object updateConfigurationRequestStatusAs {

    def query(user: User, mid: ConfigurationRequest.Id, status: ConfigurationRequestStatus): String =
      s"""
        mutation {
          updateConfigurationRequests(input: {
            SET: { status: ${status.tag.toUpperCase} }
            WHERE: { id: { EQ: ${mid.asJson} } }
          }) {
            requests {
              id
              status
            }
            hasMore
          }
        }
      """

    def apply(user: User, mid: ConfigurationRequest.Id, status: ConfigurationRequestStatus): IO[Unit] =
      expect(
        user = user,
        query = query(user, mid, status),
        expected = Right(json"""
          {
            "updateConfigurationRequests" : {
              "requests" : [
                {
                  "id" : $mid,
                  "status" : $status
                }
              ],
              "hasMore" : false
            }          
          }
        """)
      )

  }

  val setup: IO[ConfigurationRequest.Id] =
    for     
      cid <- createCallForProposalsAs(admin)
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, Some(cid), None, "Foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      mid <- createConfigurationRequestAs(pi, oid)
    yield mid

  ConfigurationRequestStatus.values.foreach: status =>
    test(s"Admin should be able to set status to $status."):
      setup.flatMap(updateConfigurationRequestStatusAs(admin, _, status))

  List(ConfigurationRequestStatus.Requested, ConfigurationRequestStatus.Withdrawn).foreach: status =>
    test(s"PI should be able to set status to $status."):
      setup.flatMap(updateConfigurationRequestStatusAs(pi, _, status))

  List(ConfigurationRequestStatus.Approved, ConfigurationRequestStatus.Denied).foreach: status =>
    test(s"PI should *not* be able to set status to $status."):
      interceptOdbError(setup.flatMap(updateConfigurationRequestStatusAs(pi, _, status))):
        case OdbError.NotAuthorized(_, _) => () // expected

  test(s"PI can't set status on another user's request (update affects no rows)"):
    setup.flatMap: mid =>
      expect(
        user = pi2,
        query = updateConfigurationRequestStatusAs.query(pi2, mid, ConfigurationRequestStatus.Withdrawn),
        expected = Right(json"""
          {
            "updateConfigurationRequests" : {
              "requests" : [],
              "hasMore" : false
            }
          }
        """)
      )

}
