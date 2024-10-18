// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.EditType
import lucuma.odb.graphql.query.ObservingModeSetupOperations

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

  def updateConfigurationRequestStatusAs(user: User, rid: ConfigurationRequest.Id, status: ConfigurationRequestStatus): IO[Unit] =
    expect(
      user = user,
      query = 
        s"""
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
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, Some(cid), None, "Foo")
      _   <- addPartnerSplits(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
    yield (pid, oid)

  test("insert and update") {
    subscriptionExpect(
      user      = pi,
      query     = subscription(None),
      mutations =
        Right(
          for
            (pid, oid) <- setup
            rid        <- createConfigurationRequestAs(pi, oid)
            _          <- updateConfigurationRequestStatusAs(pi, rid, ConfigurationRequestStatus.Withdrawn)
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
            _          <- setProposalStatus(pi, pid, "NOT_SUBMITTED")
          yield ()
        ),
      expected = List(
        configurationRequestEdit(EditType.Created, ConfigurationRequestStatus.Requested),
        json"""
          {
            "configurationRequestEdit" : {
              "editType" : "DELETED_CAL",
              "configurationRequest" : null
            }
          }
        """
      )
    )    
  }

}
