// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.User
import lucuma.odb.data.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.odb.graphql.mutation.UpdateConstraintSetOps

class observation_configurationRequests 
  extends OdbSuite 
     with ObservingModeSetupOperations 
     with UpdateConstraintSetOps {

  val pi       = TestUsers.Standard.pi(1, 30)
  val admin    = TestUsers.Standard.admin(2, 31)
  val validUsers = List(pi, admin)

  def setExplicitBaseAs(user: User, oid: Observation.Id, hms: String, dms: String): IO[Unit] =
    updateObservation(
      user = user, 
      oid = oid,
      update = s"""
        targetEnvironment: {
          explicitBase: {
            ra: { hms: "$hms"},
            dec: { dms: "$dms"}
          }
        }
      """,
      query = """
        observations {
          id
        }
      """,
      expected = Right(json"""
        {
          "updateObservations": {
            "observations": [
              {
                "id": $oid
              }
            ]
          }
        }
      """)
    )

  def expectRequests(user: User, oid: Observation.Id, ids: List[ConfigurationRequest.Id]): IO[Unit] =
    expect(
      user = user,
      query = s"""
        query {
          observation(observationId: "$oid") {
            configurationRequests {
              id
            }
          }
        }
      """,
      expected = Right(json"""                
        {
          "observation" : {
            "configurationRequests" : ${ids.map(id => Json.obj("id" -> id.asJson))}
          }
        }                
      """)
    )

  test("create and select some configuration requests"):
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None, "Foo")
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      ids   <- createConfigurationRequestAs(pi, oid).replicateA(2)
      _     <- expectRequests(pi, oid, ids)
    yield ()

  test("configuration request made for one observation should apply to another identical observation"):
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None, "Foo")
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      rid   <- createConfigurationRequestAs(pi, oid)
      oid2  <- cloneObservationAs(pi, oid)
      _     <- expectRequests(pi, oid2, List(rid))
    yield ()

  test("configuration request made for one observation should not apply to an observation with a faraway base position"):
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None, "Foo")
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      rid   <- createConfigurationRequestAs(pi, oid)
      oid2  <- cloneObservationAs(pi, oid)
      _     <- setExplicitBaseAs(pi, oid2, "1:23:45", "6:01:23")
      _     <- expectRequests(pi, oid2, Nil)
    yield ()

  test("configuration request made for one observation should apply to an observation with a nearby base position"):
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None, "Foo")
      tid   <- createTargetWithProfileAs(pi, pid)
      oid1  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _     <- setExplicitBaseAs(pi, oid1, "1:00:00", "2:00:00")
      oid2  <- cloneObservationAs(pi, oid1)
      _     <- setExplicitBaseAs(pi, oid2, "1:00:00.01", "2:00:00.01")
      rid   <- createConfigurationRequestAs(pi, oid1)
      _     <- expectRequests(pi, oid2, List(rid))
    yield ()

}
