// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.implicits.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.User
import lucuma.odb.graphql.mutation.UpdateObservationsOps

class configurationRequests 
  extends OdbSuite 
     with ObservingModeSetupOperations 
     with UpdateObservationsOps {

  val pi1   = TestUsers.Standard.pi(1, 30)
  val pi2   = TestUsers.Standard.pi(2, 31)
  val admin = TestUsers.Standard.admin(3, 32)
  val validUsers = List(pi1, pi2, admin)

  private def expectRequests(user: User, ids: List[ConfigurationRequest.Id]): IO[Unit] =
    expect(
      user = user,
      query = s"""
        query {
          configurationRequests(
            WHERE: {
              id: {
                IN: ${ids.asJson}
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """,
      expected = Right(json"""                
        {
          "configurationRequests" : {
            "matches": ${
              ids.sorted.map { id => json"""{ "id": $id }""" }
            }
          }
        }                
      """)
    )

  // set up cfp, program, and fully configured observation
  private def setupAs(user: User): IO[ConfigurationRequest.Id] =
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(user, "Foo")
      _     <- addProposal(user, pid, Some(cfpid), None)
      tid   <- createTargetWithProfileAs(user, pid)
      oid   <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      rid   <- createConfigurationRequestAs(user, oid)
    yield rid

  test("create and select some configuration requests as different users"):
    for
      mine   <- setupAs(pi1).replicateA(5)
      theirs <- setupAs(pi2).replicateA(5)
      _      <- expectRequests(pi1, mine)
      _      <- expectRequests(pi2, theirs)
    yield ()

}
