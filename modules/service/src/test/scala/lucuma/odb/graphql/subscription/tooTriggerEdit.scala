// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.TooActivation
import lucuma.core.enums.TooActivation.Interrupting
import lucuma.core.enums.TooActivation.Rapid
import lucuma.core.enums.TooActivation.Standard
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.odb.data.EditType
import lucuma.odb.data.TooTrigger
import lucuma.odb.data.TooTriggerStatus
import lucuma.odb.data.TooTriggerStatus.*

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
          value { status tooActivation }
          observation { id }
        }
      }
    """

  private def liveTriggerId(oid: Observation.Id): IO[TooTrigger.Id] =
   getRequestedTooTriggerAs(pi, oid).map(_.id)

  private def tooTriggerEdit(
    editType:   EditType,
    status:     TooTriggerStatus,
    rid:        TooTrigger.Id,
    oid:        Observation.Id,
    activation: TooActivation = TooActivation.Rapid
  ): Json =
    json"""
      {
        "tooTriggerEdit": {
          "editType":     ${editType.tag.toUpperCase},
          "tooTriggerId": $rid,
          "value":        {
            "status": ${status.tag.toScreamingSnakeCase},
            "tooActivation": ${activation.tag.toScreamingSnakeCase}
          },
          "observation":  { "id": $oid }
        }
      }
    """

  test("triggering then declining emits creation and update events"):
    (Deferred[IO, Observation.Id], Deferred[IO, TooTrigger.Id]).tupled.flatMap: (oidRef, ridRef) =>
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
              tooTriggerEdit(EditType.Created, Requested, rid, oid),
              tooTriggerEdit(EditType.Updated, Declined,  rid, oid)
            )
      )

  private def subscriptionWith(where: String): String =
    s"""
      subscription {
        tooTriggerEdit(input: { tooActivation: $where }) {
          editType
          tooTriggerId
          value { status tooActivation }
          observation { id }
        }
      }
    """

  test("a supersession arrives as a closing update and a fresh creation"):
    (Deferred[IO, Observation.Id], Deferred[IO, TooTrigger.Id], Deferred[IO, TooTrigger.Id]).tupled.flatMap: (oidRef, firstRef, secondRef) =>
      subscriptionExpectF(
        user      = pi,
        query     = subscription,
        mutations =
          Right(
            for
              (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
              _           <- oidRef.complete(oid)
              _           <- setTooWorkflowState(pi, oid, ObservationWorkflowState.Ready)
              first       <- liveTriggerId(oid)
              _           <- firstRef.complete(first)
              _           <- IO.sleep(2.seconds) // give the client time to receive the creation
              _           <- setSchedulingModeAs(pi, oid, SchedulingMode.Unconstrained)
              second      <- liveTriggerId(oid)
              _           <- secondRef.complete(second)
            yield ()
          ),
        expectedF =
          (oidRef.get, firstRef.get, secondRef.get).mapN: (oid, first, second) =>
            List(
              tooTriggerEdit(EditType.Created, Requested,  first,  oid, Rapid),
              // The predecessor closes out reporting its *own* activation, not the
              // successor's -- the value is fixed at creation.
              tooTriggerEdit(EditType.Updated, Superseded, first,  oid, Rapid),
              tooTriggerEdit(EditType.Created, Requested,  second, oid, Standard)
            )
      )

  test("the activation filter selects which events arrive"):
    (Deferred[IO, Observation.Id], Deferred[IO, TooTrigger.Id]).tupled.flatMap: (oidRef, ridRef) =>
      subscriptionExpectF(
        user      = pi,
        query     = subscriptionWith("{ EQ: STANDARD }"),
        mutations =
          Right(
            for
              (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
              _           <- oidRef.complete(oid)
              // Both of these events carry RAPID, so neither is delivered.
              _           <- setTooWorkflowState(pi, oid, ObservationWorkflowState.Ready)
              _           <- IO.sleep(2.seconds)
              _           <- setSchedulingModeAs(pi, oid, SchedulingMode.Unconstrained)
              rid         <- liveTriggerId(oid)
              _           <- ridRef.complete(rid)
            yield ()
          ),
        expectedF =
          (oidRef.get, ridRef.get).mapN: (oid, rid) =>
            List(
              // Only the successor matches: it is the one requested at STANDARD.
              tooTriggerEdit(EditType.Created, Requested, rid, oid, Standard)
            )
      )

  test("an ordered activation filter follows only the disruptive requests"):
    (Deferred[IO, Observation.Id], Deferred[IO, TooTrigger.Id], Deferred[IO, TooTrigger.Id]).tupled.flatMap: (oidRef, firstRef, thirdRef) =>
      subscriptionExpectF(
        user      = pi,
        // The whole point of matching in memory rather than in SQL: a subscription
        // can ask an ordered question.
        query     = subscriptionWith("{ GTE: RAPID }"),
        mutations =
          Right(
            for
              (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
              _           <- oidRef.complete(oid)
              _           <- setTooWorkflowState(pi, oid, ObservationWorkflowState.Ready)
              first       <- liveTriggerId(oid)
              _           <- firstRef.complete(first)
              _           <- IO.sleep(2.seconds)
              // Down to STANDARD: the predecessor's closing event still carries
              // RAPID and is delivered, but the successor's creation is not.
              _           <- setSchedulingModeAs(pi, oid, SchedulingMode.Unconstrained)
              _           <- IO.sleep(2.seconds)
              // Back up to INTERRUPTING: the STANDARD row closes out below the
              // threshold and is filtered, while its successor is delivered.
              _           <- setSchedulingModeAs(pi, oid, SchedulingMode.Interrupting)
              third       <- liveTriggerId(oid)
              _           <- thirdRef.complete(third)
            yield ()
          ),
        expectedF =
          (oidRef.get, firstRef.get, thirdRef.get).mapN: (oid, first, third) =>
            List(
              tooTriggerEdit(EditType.Created, Requested,  first, oid, Rapid),
              tooTriggerEdit(EditType.Updated, Superseded, first, oid, Rapid),
              tooTriggerEdit(EditType.Created, Requested,  third, oid, Interrupting)
            )
      )
