// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.model.User

class program_configurationRequests extends OdbSuite with ObservingModeSetupOperations {

  val pi       = TestUsers.Standard.pi(1, 30)
  val admin    = TestUsers.Standard.admin(2, 31)
  val validUsers = List(pi, admin)

  test("create and select some configuration requests") {

    def select(user: User, pid: Program.Id, ids: List[ConfigurationRequest.Id]): IO[Unit] =
      expect(
        user = user,
        query = s"""
          query {
            program(programId: "$pid") {
              configurationRequests {
                matches {
                  id
                }
              }
            }
          }
        """,
        expected = Right(json"""                
          {
            "program" : {
              "configurationRequests" : {
                "matches" : ${ids.map(id => Json.obj("id" -> id.asJson))}
              }
            }
          }                
        """)
      )

    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None, "Foo")
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      ids   <- createConfigurationRequestAs(pi, oid).replicateA(2)
      _     <- select(pi, pid, ids.distinct)
    yield ()

  }


}
