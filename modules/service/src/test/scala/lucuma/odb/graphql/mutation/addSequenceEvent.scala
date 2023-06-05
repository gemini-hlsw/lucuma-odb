// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.data.ObservingModeType


class addSequenceEvent extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def recordVisit(
    mode: ObservingModeType,
    user: User
  ):IO[(Observation.Id, Visit.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
    } yield (oid, vid)

  private def addSequenceEventTest(
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

  test("addSequenceEvent") {
    def query(vid: Visit.Id): String =
      s"""
        mutation {
          addSequenceEvent(input: {
            visitId: "$vid",
            command: START
          }) {
            event {
              visitId
              observation {
                id
              }
            }
          }
        }
      """

    addSequenceEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => query(vid),
      (oid, vid) => json"""
      {
        "addSequenceEvent": {
          "event": {
            "visitId": $vid,
            "observation": {
              "id": $oid
            }
          }
        }
      }
      """.asRight
    )

  }

  test("addSequenceEvent - unknown visit") {
    def query: String =
      s"""
        mutation {
          addSequenceEvent(input: {
            visitId: "v-b00814f4-3106-4bd9-9a05-ecdef7ef5d49",
            command: START
          }) {
            event {
              visitId
              observation {
                id
              }
            }
          }
        }
      """

    addSequenceEventTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      _ => query,
      (_, _) => s"Visit id 'v-b00814f4-3106-4bd9-9a05-ecdef7ef5d49' not found".asLeft
    )

  }


}
