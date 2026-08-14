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
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.EditType

import scala.concurrent.duration.*

class tooTriggerEdit extends OdbSuite with SubscriptionUtils with TooTriggerSetupOperations:

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(2, 32)

  val validUsers = List(pi, staff)

  private val subscription: String =
    s"""
      subscription {
        tooTriggerEdit {
          editType
          tooTriggerId
          value { status }
          observation { id }
        }
      }
    """

  private def liveTriggerId(oid: Observation.Id): IO[String] =
    query(
      pi,
      s"""
        query {
          tooTriggers(WHERE: { observationId: { EQ: ${oid.asJson} }, status: { EQ: REQUESTED } }) {
            matches { id }
          }
        }
      """
    ).map(_.hcursor.downFields("tooTriggers", "matches").require[List[Json]].head.hcursor.downField("id").require[String])

  private def declineTooTrigger(user: User, rid: String): IO[Unit] =
    query(
      user  = user,
      query = s"""
        mutation {
          declineTooTrigger(input: { tooTriggerId: "$rid" }) {
            tooTrigger { id }
          }
        }
      """
    ).void

  private def tooTriggerEdit(editType: EditType, status: String, rid: String, oid: Observation.Id): Json =
    json"""
      {
        "tooTriggerEdit": {
          "editType":     ${editType.tag.toUpperCase},
          "tooTriggerId": $rid,
          "value":        { "status": $status },
          "observation":  { "id": $oid }
        }
      }
    """

  test("triggering then declining emits creation and update events"):
    (Deferred[IO, Observation.Id], Deferred[IO, String]).tupled.flatMap: (oidRef, ridRef) =>
      subscriptionExpectF(
        user      = pi,
        query     = subscription,
        mutations =
          Right(
            for
              (_, oid, _) <- createTooObservationAs(pi, staff)
              _        <- oidRef.complete(oid)
              _        <- setTooWorkflowState(pi, oid, ObservationWorkflowState.Ready)
              rid      <- liveTriggerId(oid)
              _        <- ridRef.complete(rid)
              _        <- IO.sleep(2.seconds) // give the client time to receive the event
              _        <- declineTooTrigger(staff, rid)
            yield ()
          ),
        expectedF =
          (oidRef.get, ridRef.get).mapN: (oid, rid) =>
            List(
              tooTriggerEdit(EditType.Created, "REQUESTED", rid, oid),
              tooTriggerEdit(EditType.Updated, "DECLINED",  rid, oid)
            )
      )
