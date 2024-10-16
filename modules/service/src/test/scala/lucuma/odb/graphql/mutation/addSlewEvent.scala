// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit

class addSlewEvent extends OdbSuite {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordVisit(
    mode: ObservingModeType,
    user: User
  ):IO[(Observation.Id, Visit.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
    } yield (oid, vid)

  private def addSlewEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Visit.Id => String,
    expected: (Observation.Id, Visit.Id) => Either[String, Json]
  ): IO[Unit] = {
    for {
      ids <- recordVisit(mode, user)
      (oid, vid) = ids
      _   <- expect(user, query(vid), expected(oid, vid).leftMap(s => List(s)))
    } yield ()
}

  test("addSlewEvent") {
    def query(vid: Visit.Id): String =
      s"""
        mutation {
          addSlewEvent(input: {
            visitId: "$vid",
            slewStage: START_SLEW
          }) {
            event {
              eventType
              visit {
                id
              }
              observation {
                id
              }
              ... on SlewEvent {
                slewStage
              }
            }
          }
        }
      """

    addSlewEventTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      vid => query(vid),
      (oid, vid) => json"""
      {
        "addSlewEvent": {
          "event": {
            "eventType": "SLEW",
            "visit": {
              "id": $vid
            },
            "observation": {
              "id": $oid
            },
            "slewStage": "START_SLEW"
          }
        }
      }
      """.asRight
    )

  }

  test("addSlewEvent - unknown visit") {
    def query: String =
      s"""
        mutation {
          addSlewEvent(input: {
            visitId: "v-42",
            slewStage: START_SLEW
          }) {
            event {
              visit {
                id
              }
              observation {
                id
              }
            }
          }
        }
      """

    addSlewEventTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      _ => query,
      (_, _) => s"Visit 'v-42' not found".asLeft
    )

  }


}
