// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User

class requestTooTrigger extends OdbSuite with TooTriggerSetupOperations {

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(2, 32)

  val validUsers = List(pi, staff)

  private def requestTooTrigger(user: User, oid: Observation.Id, child: String): IO[Json] =
    query(
      user  = user,
      query = s"""
        mutation {
          requestTooTrigger(input: {
            observationId: "$oid"
          }) {
            tooTrigger {
              $child
            }
          }
        }
      """
    )

  test("request a trigger and select its mapped fields") {
    createTooObservationAs(pi, staff).flatMap { (_, oid) =>
        expect(
          user  = pi,
          query = s"""
            mutation {
              requestTooTrigger(input: {
                observationId: "$oid"
              }) {
                tooTrigger {
                  observation { id }
                  status
                  resolutionReason
                  requestedBy { id }
                }
              }
            }
          """,
          expected = Right(json"""
            {
              "requestTooTrigger" : {
                "tooTrigger" : {
                  "observation" : { "id" : $oid },
                  "status" : "REQUESTED",
                  "resolutionReason" : null,
                  "requestedBy" : { "id" : ${pi.id} }
                }
              }
            }
          """)
        )
    }
  }

  test("requested trigger can be read back via the tooTrigger query") {
    for
      (pid, oid) <- createTooObservationAs(pi, staff)
      rid <- requestTooTrigger(pi, oid, "id").map(_.hcursor.downFields("requestTooTrigger", "tooTrigger", "id").require[String])
      _   <- expect(
               user  = pi,
               query = s"""
                 query {
                   tooTrigger(tooTriggerId: "$rid") {
                     id
                     observation { id }
                     status
                   }
                 }
               """,
               expected = Right(json"""
                 {
                   "tooTrigger" : {
                     "id" : $rid,
                     "observation" : { "id" : $oid },
                     "status" : "REQUESTED"
                   }
                 }
               """)
             )
    yield ()
  }

  test("staff can accept a requested trigger") {
    for
      (pid, oid) <- createTooObservationAs(pi, staff)
      rid <- requestTooTrigger(pi, oid, "id").map(_.hcursor.downFields("requestTooTrigger", "tooTrigger", "id").require[String])
      _   <- expect(
               user  = staff,
               query = s"""
                 mutation {
                   acceptTooTrigger(input: {
                     tooTriggerId: "$rid"
                   }) {
                     tooTrigger {
                       id
                       status
                       resolutionReason
                     }
                   }
                 }
               """,
               expected = Right(json"""
                 {
                   "acceptTooTrigger" : {
                     "tooTrigger" : {
                       "id" : $rid,
                       "status" : "ACCEPTED",
                       "resolutionReason" : null
                     }
                   }
                 }
               """)
             )
    yield ()
  }

}
