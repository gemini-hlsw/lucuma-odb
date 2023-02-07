// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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

class targets extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service).toList

  test("simple target selection") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).replicateA(5).flatMap { tids =>
        expect(
          user = pi,
          query = s"""
            query {
              targets(
                WHERE: {
                  programId: {
                    EQ: ${pid.asJson}
                  }
                }
              ) {
                hasMore
                matches {
                  id
                }
              }
            }
          """,
          expected =
            Right(Json.obj(
              "targets" -> Json.obj(
                "hasMore" -> Json.False,
                "matches" -> Json.fromValues(
                    tids.map { id =>
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

  test("simple target selection with limit (more)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).replicateA(5).flatMap { tids =>
        expect(
          user = pi,
          query = s"""
            query {
              targets(
                WHERE: {
                  programId: {
                    EQ: ${pid.asJson}
                  }
                }
                LIMIT: 3
              ) {
                hasMore
                matches {
                  id
                }
              }
            }
          """,
          expected =
            Right(Json.obj(
              "targets" -> Json.obj(
                "hasMore" -> Json.True,
                "matches" -> Json.fromValues(
                    tids.take(3).map { id =>
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

  test("target selection with offset and limit") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).replicateA(10).flatMap { tids =>
        expect(
          user = pi,
          query = s"""
            query {
              targets(
                WHERE: {
                  programId: {
                    EQ: ${pid.asJson}
                  }
                }
                OFFSET: ${tids(3).asJson}
                LIMIT: 3
              ) {
                hasMore
                matches {
                  id
                }
              }
            }
          """,
          expected =
            Right(Json.obj(
              "targets" -> Json.obj(
                "hasMore" -> Json.True,
                "matches" -> Json.fromValues(
                    tids.drop(3).take(3).map { id =>
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

  test("target selection with multiple filters") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).replicateA(5).flatMap { tids =>
        expect(
          user = pi,
          query = s"""
            query {
              targets(
                WHERE: {
                  id: {
                    NEQ: ${tids(3).asJson}
                  }
                  programId: {
                    EQ: ${pid.asJson}
                  }
                }
              ) {
                matches {
                  id
                }
              }
            }
          """,
          expected =
            Right(Json.obj(
              "targets" -> Json.obj(
                "matches" -> Json.fromValues {
                  tids.filterNot(_ === tids(3)).map { id =>
                    Json.obj("id" -> id.asJson)
                  }
                }
              )
            ))
        )
      }
    }
  }

}