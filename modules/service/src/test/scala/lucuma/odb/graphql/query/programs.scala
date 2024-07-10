// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.User
import lucuma.odb.data.CalibrationRole
import lucuma.odb.graphql.input.ProgramPropertiesInput

class programs extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service).toList

  test("simple program selection") {
    createProgramAs(pi).replicateA(5).flatMap { pids =>
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
    createProgramAs(pi2).replicateA(5).flatMap { pids =>
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

  test("calibration program selection") {
    for {
      pid  <- withServices(service) { s =>
                s.session.transaction.use { xa =>
                  s.programService
                    .insertCalibrationProgram(
                      ProgramPropertiesInput.Create(None).some,
                      CalibrationRole.Telluric,
                      Description.unsafeFrom("TELLURIC2"))(using xa)
                }
              }
      _    <- expect(
            user = service,
            query = s"""
              query {
                programs(
                  WHERE: {
                    calibrationRole: {
                      IN: [TELLURIC]
                    }
                  }
                ) {
                  matches {
                    id
                    calibrationRole
                  }
                }
              }
            """,
            expected =
              Right(json"""
                {
                  "programs": {
                    "matches": [
                      {
                        "id": ${pid.asJson},
                        "calibrationRole": "TELLURIC"
                      }
                    ]
                  }
                }
                """
              )
            )
    } yield ()
  }
}
