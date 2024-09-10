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

  // set up cfp, program, and fullt configured observation
  def setup: IO[Observation.Id] =
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None, "Foo")
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
    yield oid

  test("create and select some configuration requests"):
    for
      oid   <- setup
      ids   <- createConfigurationRequestAs(pi, oid).replicateA(2)
      _     <- expectRequests(pi, oid, ids)
    yield ()

  test("request should apply to identical obs"):
    for
      oid   <- setup
      oid2  <- cloneObservationAs(pi, oid)
      rid   <- createConfigurationRequestAs(pi, oid)
      _     <- expectRequests(pi, oid2, List(rid))
    yield ()

  test("request should not apply for faraway base position"):
    for
      oid  <- setup
      _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
      rid  <- createConfigurationRequestAs(pi, oid)
      _    <- setExplicitBaseAs(pi, oid, "3:00:00", "4:00:00")
      _    <- expectRequests(pi, oid, Nil)
    yield ()

  test("request should reappear when base position is moved back"):
    for
      oid  <- setup
      _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
      rid  <- createConfigurationRequestAs(pi, oid)
      _    <- setExplicitBaseAs(pi, oid, "3:00:00", "4:00:00")
      _    <- expectRequests(pi, oid, Nil) // sanity check
      _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
      _    <- expectRequests(pi, oid, List(rid))
    yield ()

  test("request should apply to obs with nearby base position"):
    for
      oid  <- setup
      _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
      rid  <- createConfigurationRequestAs(pi, oid)
      _    <- setExplicitBaseAs(pi, oid, "1:00:00.01", "2:00:00.01")
      _    <- expectRequests(pi, oid, List(rid))
    yield ()

}
