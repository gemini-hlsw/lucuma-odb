// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import cats.effect.IO
import lucuma.odb.data.ConfigurationRequest
import lucuma.core.model.User

class updateConfigurationRequests extends OdbSuite with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val admin   = TestUsers.Standard.admin(2, 32)

  val validUsers = List(pi, admin).toList

  def updateConfigurationRequestStatusAs(user: User, rid: ConfigurationRequest.Id, status: ConfigurationRequest.Status): IO[Unit] =
    expect(
      user = admin,
      query = s"""
        mutation {
          updateConfigurationRequests(input: {
            SET: { status: ${status.tag.toUpperCase} }
            WHERE: { id: { EQ: ${rid.asJson} } }
          }) {
            requests {
              id
              status
            }
            hasMore
          }
        }
      """,
      expected = Right(json"""
        {
          "updateConfigurationRequests" : {
            "requests" : [
              {
                "id" : $rid,
                "status" : $status
              }
            ],
            "hasMore" : false
          }          
        }
      """)
    )

  val setup: IO[ConfigurationRequest.Id] =
    for     
      cid <- createCallForProposalsAs(admin)
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, Some(cid), None, "Foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      rid <- createConfigurationRequestAs(pi, oid)
    yield rid

  ConfigurationRequest.Status.values.foreach: status =>
    test(s"Admin should be able to set status to $status."):
      setup.flatMap(updateConfigurationRequestStatusAs(admin, _, status))

}
