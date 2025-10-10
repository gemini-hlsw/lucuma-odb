// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.WaterVapor
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.EditType
import lucuma.odb.graphql.query.ObservingModeSetupOperations

import scala.concurrent.duration.*

class configurationRequestEdit extends OdbSuite with SubscriptionUtils with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val admin   = TestUsers.Standard.admin(2, 32)
  val validUsers = List(pi, admin).toList

  def setProposalStatusAs(user: User, pid: Program.Id, status: String): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          setProposalStatus(
            input: {
              programId: ${pid.asJson}
              status: $status
            }
          ) {
            program {
              configurationRequests {
                matches {
                  status
                }
              }
            }
          }
        }
      """,
      expected =
        json"""
          {
            "setProposalStatus": {
              "program": {
                "configurationRequests" : {
                  "matches" : [
                    {
                      "status" : ${status.asJson}
                    }
                  ]
                }
              }
            }
          }
        """.asRight
    )

  def updateConfigurationRequestStatusAs(user: User, mid: ConfigurationRequest.Id, status: ConfigurationRequestStatus): IO[Unit] =
    expect(
      user = user,
      query = 
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
        """,
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


  def subscription(
    pid: Option[Program.Id],
  ): String = {
    val args: String =
      pid.foldMap(p => s"(input: { programId: ${p.asJson} })")

    s"""
      subscription {
        configurationRequestEdit$args {
          editType
          configurationRequest {
            status
          }
        }
      }
    """
  }

  def configurationRequestEdit(
    editType: EditType, status: ConfigurationRequestStatus
  ): Json =
    Json.obj(
      "configurationRequestEdit" -> Json.obj(
        "editType" -> Json.fromString(editType.tag.toUpperCase),
        "configurationRequest" -> Json.obj(
          "status" -> status.asJson
        )
      )
    )

  val setup: IO[(Program.Id, Observation.Id)] =
    for     
      cid <- createCallForProposalsAs(admin)
      pid <- createProgramWithUsPi(pi)
      _   <- addProposal(pi, pid, Some(cid), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- computeItcResultAs(pi, oid)
    yield (pid, oid)

  test("insert and update") {
    subscriptionExpect(
      user      = pi,
      query     = subscription(None),
      mutations =
        Right(
          for
            (pid, oid) <- setup
            mid        <- createConfigurationRequestAs(pi, oid)
            _          <- IO.sleep(2.seconds) // give the client time to receive the event … CI seems to need more time
            _          <- updateConfigurationRequestStatusAs(pi, mid, ConfigurationRequestStatus.Withdrawn)
          yield ()
        ),
      expected = List(
        configurationRequestEdit(EditType.Created, ConfigurationRequestStatus.Requested),
        configurationRequestEdit(EditType.Updated, ConfigurationRequestStatus.Withdrawn)
      )
    )    
  }


  test("submit proposal, then unsubmit") {
    subscriptionExpect(
      user      = pi,
      query     = subscription(None),
      mutations =
        Right(
          for
            (pid, oid) <- setup
            _          <- setProposalStatus(pi, pid, "SUBMITTED")
            _          <- IO.sleep(2.seconds) // give the client time to receive the event … CI seems to need more time
            _          <- setProposalStatus(pi, pid, "NOT_SUBMITTED")
          yield ()
        ),
      expected = List(
        configurationRequestEdit(EditType.Created, ConfigurationRequestStatus.Requested),
        json"""
          {
            "configurationRequestEdit" : {
              "editType" : "HARD_DELETE",
              "configurationRequest" : null
            }
          }
        """
      )
    )    
  }


  // https://app.shortcut.com/lucuma/story/3926/error-in-configurationrequestedit-subscription
  // I'm getting an error in the configurationRequestEdit subscription if I already have a pending request
  // under particular conditions, and then make a new request for more restrictive conditions. ie. original 
  // request for "Bright" and new request for "Dark". The error is "Internal Error: Expected single value
  // for field 'id' of type ConfigurationRequest at List(configurationRequest, configurationRequestEdit), 
  // found many" 
  test("shortcut 3926") {

    val subscription: String =
      s"""
        subscription {
          configurationRequestEdit() {
            editType
#            configurationRequestId
            configurationRequest {
              id
              status
            }
          }
        }
      """

    def configurationRequestEdit(
      rid: ConfigurationRequest.Id, editType: EditType, status: ConfigurationRequestStatus
    ): Json =
      Json.obj(
        "configurationRequestEdit" -> Json.obj(
          "editType" -> Json.fromString(editType.tag.toUpperCase),
          "configurationRequest" -> Json.obj(
            "id" -> rid.asJson,
            "status" -> status.asJson
          )
        )
      )

    def updateWaterVapor(oid: Observation.Id, wv: WaterVapor): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                constraintSet: {
                  waterVapor: ${wv.tag.toUpperCase()}
                }
              },
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              observations {
                constraintSet {
                  waterVapor
                }
              }
            }
          }
        """,
        expected =json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "constraintSet" : {
                    "waterVapor" : ${wv.asJson}
                  }
                }
              ]
            }
          }
        """.asRight
      )

    (Deferred[IO, ConfigurationRequest.Id], Deferred[IO, ConfigurationRequest.Id]).tupled.flatMap: 
      case (ref1, ref2) =>
        subscriptionExpectF(
          user      = pi,
          query     = subscription,
          mutations =
            Right(
              for
                (pid, oid) <- setup
                _          <- createConfigurationRequestAs(pi, oid).flatMap(ref1.complete)
                _          <- IO.sleep(2.seconds) // give the client time to receive the event … CI seems to need more time
                _          <- updateWaterVapor(oid, WaterVapor.Dry)
                _          <- createConfigurationRequestAs(pi, oid).flatMap((ref2.complete))
              yield ()
            ),
          expectedF = 
            (ref1.get, ref2.get).mapN: (rid1, rid2) =>
              List(
                configurationRequestEdit(rid1, EditType.Created, ConfigurationRequestStatus.Requested),
                configurationRequestEdit(rid2, EditType.Created, ConfigurationRequestStatus.Requested)
            )
          )    
      }

}
