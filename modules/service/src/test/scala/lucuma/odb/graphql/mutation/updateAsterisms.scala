// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

class updateAsterisms extends OdbSuite
                         with CreateProgramOps
                         with CreateObservationOps {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("update add to empty asterism") {

    def addToAsterism(
      user: User,
      pid:  Program.Id,
      oid:  Observation.Id,
      tid:  Target.Id
    ): IO[Unit] =
      expect(
        user  = user,
        query = s"""
          mutation {
            updateAsterisms(input: {
              programId: ${pid.asJson}
              SET: {
                ADD: [ ${tid.asJson} ]
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              targetEnvironment {
                asterism {
                  id
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "updateAsterisms": [
              {
                "targetEnvironment": {
                  "asterism": [
                    {
                      "id": ${tid.asJson}
                    }
                  ]
                }
              }
            ]
          }
        """.asRight
      )

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      tid <- createEmptyTargetAs(pi, pid, "Larry")
      _   <- addToAsterism(pi, pid, oid, tid)
    } yield ()
  }

}
