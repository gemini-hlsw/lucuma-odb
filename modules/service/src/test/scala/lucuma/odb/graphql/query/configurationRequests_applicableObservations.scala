// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.implicits.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.mutation.UpdateObservationsOps

class configurationRequests_applicableObservations
  extends OdbSuite 
     with ObservingModeSetupOperations 
     with UpdateObservationsOps {

  val pi = TestUsers.Standard.pi(1, 30)
  val admin = TestUsers.Standard.admin(3, 32)
  val validUsers = List(pi, admin)

  private def expectRequests(user: User, data: List[(ConfigurationRequest.Id, List[Observation.Id])]): IO[Unit] =
    expect(
      user = user,
      query = s"""
        query {
          configurationRequests(
            WHERE: {
              id: {
                IN: ${data.map(_._1).asJson}
              }
            }
          ) {
            matches {
              id
              applicableObservations
            }
          }
        }
      """
    ,
      expected = Right(json"""                
        {
          "configurationRequests" : {
            "matches": ${
              data.sortBy(_._1).map { case (cid, oids) => 
                json"""
                  { 
                    "id": $cid,
                    "applicableObservations": $oids
                  }
                """ 
              }
            }
          }
        }                
      """)
    )

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

  private def updateGratingAs(user: User, oid: Observation.Id, grating: GmosNorthGrating): IO[Unit] =
    updateObservationAs(user, oid):
      s"""
        observingMode: {
          gmosNorthLongSlit: {
            grating: ${grating.tag.toUpperCase()}
          }
        }
      """

  test("applicable requests"):
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi, "Foo")
      _     <- addProposal(pi, pid, Some(cfpid), None)
      tid   <- createTargetWithProfileAs(pi, pid)
      oid1  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid)) // oid1 and oid2 are identical
      oid2  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      oid3  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid)) // oid3 is different
      _     <- updateGratingAs(pi, oid3, GmosNorthGrating.R150_G5308)   // due to this change
      oid4  <- createObservationAs(pi, pid)  // oid4 has no configuration

      rid1  <- createConfigurationRequestAs(pi, oid1)
      rid2  <- createConfigurationRequestAs(pi, oid3)

      _     <- expectRequests(pi, List(
                rid1 -> List(oid1, oid2),
                rid2 -> List(oid3)
              ))
      
    yield ()


}
