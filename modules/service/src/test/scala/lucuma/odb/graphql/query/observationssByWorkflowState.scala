// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Observation

class observationssByWorkflowState extends OdbSuite {

  val pi = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi).toList

  test("simple observation selection") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid).replicateA(5).flatMap { oids =>

        IO.println("------------- TEST STARTS HERE ------------") >>
        expect(
          user = pi,
          query = s"""
            query {
              observationsByWorkflowState(
                WHERE: {
                  id: {
                    IN: ["o-101", "o-102"]
                  }
                },
                states: [UNDEFINED]
              ) {
                id
                calibrationRole
                observerNotes
              }
            }
          """,
          expected =
            Right(Json.obj(
              "observations" -> Json.obj(
                "hasMore" -> Json.False,
                "matches" -> Json.fromValues(
                    oids.map { id =>
                      Json.obj(
                        "id"              -> id.asJson,
                        "calibrationRole" -> Json.Null,
                        "observerNotes"   -> Json.Null
                      )
                    }
                )
              )
            )
          )
        )
      }
    }
  }

}
