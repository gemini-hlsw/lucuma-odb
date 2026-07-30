// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User

class tooTriggerChronicleEntries extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(2, 32)

  val validUsers = List(pi, staff)

  private def requestTooTrigger(user: User, oid: Observation.Id): IO[String] =
    query(
      user  = user,
      query = s"""
        mutation {
          requestTooTrigger(input: { observationId: "$oid" }) {
            tooTrigger { id }
          }
        }
      """
    ).map(_.hcursor.downFields("requestTooTrigger", "tooTrigger", "id").require[String])

  private def acceptTooTrigger(user: User, rid: String): IO[Unit] =
    query(
      user  = user,
      query = s"""
        mutation {
          acceptTooTrigger(input: { tooTriggerId: "$rid" }) {
            tooTrigger { id }
          }
        }
      """
    ).void

  test("request then accept generates two chronicle entries"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      rid <- requestTooTrigger(pi, oid)
      _   <- acceptTooTrigger(staff, rid)
      _   <- expect(
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
                  "modResolutionReason":  false,
                  "newObservationId":     null,
                  "newProgramId":         null,
                  "newStatus":            "ACCEPTED",
                  "newResolutionReason":  null
                }
              ]
            }
          }
        """.asRight
      )
    yield ()
