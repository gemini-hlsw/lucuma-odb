// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.mutation.UpdateConstraintSetOps

class observation_configurationRequests 
  extends OdbSuite 
     with ObservingModeSetupOperations 
     with UpdateConstraintSetOps {

  val pi       = TestUsers.Standard.pi(1, 30)
  val admin    = TestUsers.Standard.admin(2, 31)
  val validUsers = List(pi, admin)

  private def updateObservationAs(user: User, oid: Observation.Id)(update: String): IO[Unit] =
    updateObservation(user, oid, update,
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

  private def setExplicitBaseAs(user: User, oid: Observation.Id, hms: String, dms: String): IO[Unit] =
    updateObservationAs(user, oid):
      s"""
        targetEnvironment: {
          explicitBase: {
            ra: { hms: "$hms"},
            dec: { dms: "$dms"}
          }
        }
      """
  private def updateGratingAs(user: User, oid: Observation.Id, grating: GmosNorthGrating): IO[Unit] =
    updateObservationAs(user, oid):
      s"""
        observingMode: {
          gmosNorthLongSlit: {
            grating: ${grating.tag.toUpperCase()}
          }
        }
      """

  private def updateCloudExtinction(user: User, oid: Observation.Id, cloudExtinction: CloudExtinction.Preset): IO[Unit] =
    updateObservationAs(user, oid):
      s"""
        constraintSet: {
          cloudExtinction: ${cloudExtinction.tag.toUpperCase()}
        }
      """

  private def expectRequests(user: User, oid: Observation.Id, ids: List[ConfigurationRequest.Id]): IO[Unit] =
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
  private def setup: IO[Observation.Id] =
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi, "Foo")
      _     <- addProposal(pi, pid, Some(cfpid), None)
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
    yield oid

  test("create and select some configuration requests, expect deduplication"):
    for
      oid   <- setup
      ids   <- createConfigurationRequestAs(pi, oid).replicateA(2)
      _     <- expectRequests(pi, oid, ids.distinct)
    yield ()

  test("request should apply to identical obs"):
    for
      oid   <- setup
      oid2  <- cloneObservationAs(pi, oid)
      mid   <- createConfigurationRequestAs(pi, oid)
      _     <- expectRequests(pi, oid2, List(mid))
    yield ()

  test("request should apply for nearby base position"):
    for
      oid  <- setup
      _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
      mid  <- createConfigurationRequestAs(pi, oid)
      _    <- setExplicitBaseAs(pi, oid, "1:00:00.01", "2:00:00.01")
      _    <- expectRequests(pi, oid, List(mid))
    yield ()

  test("request should not apply for faraway base position"):
    for
      oid  <- setup
      _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
      mid  <- createConfigurationRequestAs(pi, oid)
      _    <- setExplicitBaseAs(pi, oid, "3:00:00", "4:00:00")
      _    <- expectRequests(pi, oid, Nil)
    yield ()

  test("request should apply when base position is moved back"):
    for
      oid  <- setup
      _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
      mid  <- createConfigurationRequestAs(pi, oid)
      _    <- setExplicitBaseAs(pi, oid, "3:00:00", "4:00:00")
      _    <- expectRequests(pi, oid, Nil) // sanity check
      _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
      _    <- expectRequests(pi, oid, List(mid))
    yield ()

  test("request should not apply for different observing mode"):
    for
      oid  <- setup
      _    <- updateGratingAs(pi, oid, GmosNorthGrating.B480_G5309)
      mid  <- createConfigurationRequestAs(pi, oid)
      _    <- updateGratingAs(pi, oid, GmosNorthGrating.R150_G5308)
      _    <- expectRequests(pi, oid, Nil)
    yield ()

  test("request should not apply for narrower conditions"):
    for
      oid  <- setup
      _    <- updateCloudExtinction(pi, oid, CloudExtinction.Preset.OnePointZero)
      mid  <- createConfigurationRequestAs(pi, oid)
      _    <- updateCloudExtinction(pi, oid, CloudExtinction.Preset.PointFive) // can't ask for better conditions
      _    <- expectRequests(pi, oid, Nil)
    yield ()

  test("request should apply for wider conditions"):
    for
      oid  <- setup
      _    <- updateCloudExtinction(pi, oid, CloudExtinction.Preset.PointFive)
      mid  <- createConfigurationRequestAs(pi, oid)
      _    <- updateCloudExtinction(pi, oid, CloudExtinction.Preset.OnePointZero) // ok to ask for worse conditions
      _    <- expectRequests(pi, oid, List(mid))
    yield ()

}
