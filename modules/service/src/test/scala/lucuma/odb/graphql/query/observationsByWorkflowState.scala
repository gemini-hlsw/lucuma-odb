// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation

class observationsByWorkflowState extends OdbSuite {

  val pi = TestUsers.Standard.pi(nextId, nextId)
  val service = TestUsers.service(nextId)
  val validUsers = List(pi, service).toList

  test("ensure outer predicate works") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid)
        .replicateA(5)  // make a bunch
        .map(_.take(2)) // but only ask for a few
        .flatMap: oids =>
          expect(
            user = service,
            query = s"""
              query {
                observationsByWorkflowState(
                  WHERE: {
                    id: {
                      IN: ${oids.asJson}
                    }
                  },
                  states: [UNDEFINED]
                ) {
                  id
                }
              }
            """,
            expected =
              Right(Json.obj(
                "observationsByWorkflowState" -> 
                    oids
                      .map: id =>
                        Json.obj(
                          "id" -> id.asJson,
                        )
                      .asJson
              ))
          )
    }
  }

  test("ensure state predicate works") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid)
        .replicateA(5)  // make a bunch 
        .flatMap: oids =>
          // set the last one to inactive
          setObservationWorkflowState(pi, oids.last, ObservationWorkflowState.Inactive) >>
          // query for all but the first one
          expect(
            user = service,
            query = s"""
              query {
                observationsByWorkflowState(
                  WHERE: {
                    id: {
                      IN: ${oids.drop(1).asJson} # filter out the first one
                    }
                  },
                  states: [UNDEFINED]
                ) {
                  id
                }
              }
            """,
            expected =
              // we should get back only the middle ones
              Right(Json.obj(
                "observationsByWorkflowState" -> 
                    oids.drop(1).dropRight(1)
                      .map: id =>
                        Json.obj(
                          "id" -> id.asJson,
                        )
                      .asJson
              ))
          )
    }
  }

}
