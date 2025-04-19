// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.StepExecutionState

trait ExecutionState { this: OdbSuite =>

  def atomExecutionState(user: User, oid: Observation.Id): IO[List[AtomExecutionState]] =
    query(
      user = user,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              atomRecords {
                matches {
                  executionState
                }
              }
            }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downFields("observation", "execution", "atomRecords", "matches")
        .values
        .toList
        .flatTraverse(_.toList.traverse { js =>
          js.hcursor
            .downField("executionState")
            .as[AtomExecutionState]
            .leftMap(f => new RuntimeException(f.message))
            .liftTo[IO]
        })
    }

  def stepExecutionState(user: User, oid: Observation.Id): IO[List[StepExecutionState]] =
    query(
      user = user,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              atomRecords {
                matches {
                  steps {
                    matches {
                      executionState
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downFields("observation", "execution", "atomRecords", "matches")
        .values
        .toList
        .flatTraverse(_.toList.flatTraverse { js =>
          js.hcursor
            .downFields("steps", "matches")
            .values
            .toList
            .flatTraverse(_.toList.traverse { js =>
              js.hcursor
                .downField("executionState")
                .as[StepExecutionState]
                .leftMap(f => new RuntimeException(f.message))
                .liftTo[IO]
            })
        })
    }

}
