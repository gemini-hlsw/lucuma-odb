// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.Target
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.service.Services

class targets extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)
  val staff   = TestUsers.Standard.staff(nextId, nextId)

  val validUsers = List(pi, pi2, staff, service).toList

  test("simple target selection") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).replicateA(5).flatMap { tids =>
        expect(
          user = pi,
          query = s"""
            query {
              targets(
                WHERE: {
                  program: {
                    id: { EQ: "$pid" }
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
                  program: {
                    id: { EQ: "$pid" }
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
                  program: {
                    id: { EQ: "$pid" }
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
                  program: {
                    id: { EQ: "$pid" }
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

  test("target selection with calibration role") {
    for {
      pid <- withServices(service) { s =>
              Services.asSuperUser:
                s.session.transaction.use { xa =>
                  s.programService(emailConfig, httpClient)
                    .insertCalibrationProgram(
                      ProgramPropertiesInput.Create.Default.some,
                      CalibrationRole.Telluric,
                      Description.unsafeFrom("TELLURIC1"))(using xa)
                }
            }
      tids <- createTargetAs(service, pid).replicateA(5)
      _ <- expect(
            user = service,
            query = s"""
              query {
                targets(
                  WHERE: {
                    calibrationRole: {
                      EQ: TELLURIC
                    }
                    program: {
                      id: { EQ: "$pid" }
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
              Right(Json.obj(
                "targets" -> Json.obj(
                  "matches" -> Json.fromValues {
                      tids.map { id =>
                        Json.obj(
                          "id" -> id.asJson,
                          "calibrationRole" -> Json.fromString("TELLURIC")
                      )
                    }
                  }
                )
              ))
          )
    } yield ()
  }

  test("target selection with mulitple calibration roles") {
    for {
      pid <- withServices(service) { s =>
              Services.asSuperUser:
                s.session.transaction.use { xa =>
                  s.programService(emailConfig, httpClient)
                    .insertCalibrationProgram(
                      ProgramPropertiesInput.Create.Default.some,
                      CalibrationRole.Photometric,
                      Description.unsafeFrom("PHOTO"))(using xa)
                }
            }
      tids <- createTargetAs(service, pid).replicateA(5)
      _    <-
          expect(
            user = service,
            query = s"""
              query {
                targets(
                  WHERE: {
                    calibrationRole: {
                      IN: [PHOTOMETRIC, TELLURIC]
                    }
                    program: {
                      id: { EQ: "$pid" }
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
              Right(Json.obj(
                "targets" -> Json.obj(
                  "matches" -> Json.fromValues {
                      tids.map { id =>
                        Json.obj(
                          "id" -> id.asJson,
                          "calibrationRole" -> Json.fromString("PHOTOMETRIC")
                      )
                    }
                  }
                )
              ))
          )
    } yield ()
  }

  test("target selection without a specific calibration role") {
    for {
      pid  <- withServices(service) { s =>
                Services.asSuperUser:
                  s.session.transaction.use { xa =>
                    s.programService(emailConfig, httpClient)
                      .insertCalibrationProgram(
                        ProgramPropertiesInput.Create.Default.some,
                        CalibrationRole.Telluric,
                        Description.unsafeFrom("TELLURIC2"))(using xa)
                  }
              }
      tids <- createTargetAs(service, pid).replicateA(5)
      _    <- expect(
            user = service,
            query = s"""
              query {
                targets(
                  WHERE: {
                    calibrationRole: {
                      NIN: [PHOTOMETRIC]
                    }
                    program: {
                      id: { EQ: "$pid" }
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
              Right(Json.obj(
                "targets" -> Json.obj(
                  "matches" -> Json.fromValues {
                      // Only Tellric targets
                      tids.map { id =>
                        Json.obj(
                          "id" -> id.asJson,
                          "calibrationRole" -> Json.fromString("TELLURIC")
                      )
                    }
                  }
                )
              ))
          )
    } yield ()
  }
}
