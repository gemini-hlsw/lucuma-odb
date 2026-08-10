// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.User

class tooTriggerChronicleEntries extends OdbSuite with TooTriggerSetupOperations:

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(2, 32)

  val validUsers = List(pi, staff)

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

  private def declineTooTrigger(user: User, rid: String, reason: String): IO[Unit] =
    query(
      user  = user,
      query = s"""
        mutation {
          declineTooTrigger(input: { tooTriggerId: "$rid", reason: "$reason" }) {
            tooTrigger { id }
          }
        }
      """
    ).void

  test("triggering then declining generates two chronicle entries"):
    for
      (pid, oid) <- createTooObservationAs(pi, staff)
      _          <- setTooWorkflowState(pi, oid, ObservationWorkflowState.Ready)
      rid        <- liveTriggerId(oid)
      _          <- declineTooTrigger(staff, rid, "too faint")
      _          <- expect(
        staff,
        s"""
          query {
            tooTriggerChronicleEntries(WHERE: {
              tooTrigger: { EQ: "$rid" }
            }) {
              hasMore
              matches {
                operation
                tooTrigger { id }
                modObservationId
                modProgramId
                modStatus
                modResolutionReason
                newObservationId
                newProgramId
                newStatus
                newResolutionReason
              }
            }
          }
        """,
        json"""
          {
            "tooTriggerChronicleEntries": {
              "hasMore": false,
              "matches": [
                {
                  "operation":            "INSERT",
                  "tooTrigger":           { "id": $rid },
                  "modObservationId":     true,
                  "modProgramId":         true,
                  "modStatus":            true,
                  "modResolutionReason":  false,
                  "newObservationId":     $oid,
                  "newProgramId":         $pid,
                  "newStatus":            "REQUESTED",
                  "newResolutionReason":  null
                },
                {
                  "operation":            "UPDATE",
                  "tooTrigger":           { "id": $rid },
                  "modObservationId":     false,
                  "modProgramId":         false,
                  "modStatus":            true,
                  "modResolutionReason":  true,
                  "newObservationId":     null,
                  "newProgramId":         null,
                  "newStatus":            "DECLINED",
                  "newResolutionReason":  "too faint"
                }
              ]
            }
          }
        """.asRight
      )
    yield ()
