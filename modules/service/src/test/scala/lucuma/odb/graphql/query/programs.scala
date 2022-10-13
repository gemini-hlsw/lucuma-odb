// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class programs extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service).toList

  def createProgram(user: User): IO[Program.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createProgram(input: { SET: { name: "${user.displayName}'s Program" } }) {
              program {
                id
              }
            }
          }
        """
    ) map { json =>
      json.hcursor.downFields("createProgram", "program", "id").require[Program.Id]
    }

  test("simple program selection") {
    createProgram(pi).replicateA(5).flatMap { pids =>
      expect(
        user = pi,
        query = s"""
          query {
            programs() {
              hasMore
              matches {
                id
              }
            }
          }
        """,
        expected =
          Right(Json.obj(
            "programs" -> Json.obj(
              "hasMore" -> Json.False,
              "matches" -> Json.fromValues(
                  pids.map { id =>
                    Json.obj("id" -> id.asJson)
                  }
              )
            )
          )
        )              
      )
    }
  }

  test("simple program selection with limit") {
    createProgram(pi2).replicateA(5).flatMap { pids =>
      expect(
        user = pi2,
        query = s"""
          query {
            programs(LIMIT: 4) {
              hasMore
              matches {
                id
              }
            }
          }
        """,
        expected =
          Right(Json.obj(
            "programs" -> Json.obj(
              "hasMore" -> Json.True,
              "matches" -> Json.fromValues(
                  pids.take(4).map { id =>
                    Json.obj("id" -> id.asJson)
                  }
              )
            )
          )
        )              
      )
    }
  }

}
