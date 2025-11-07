// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Partner
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.StandardRole
import lucuma.core.util.Gid
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.service.Services

import java.time.LocalDate

class programs extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val guest1  = TestUsers.guest(3)
  val guest2  = TestUsers.guest(4)
  val staff   = TestUsers.Standard.staff(5, 34)

  val piCharles = TestUsers.Standard(
    6,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(6).get),
    email = "charles@guiteau.com".some
  )

  val piLeon    = TestUsers.Standard(
    7,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(7).get),
    email = "leon@czolgosz.edu".some
  )

  val service = TestUsers.service(10)

  val validUsers = List(pi, pi2, guest1, guest2, staff, piCharles, piLeon, service).toList

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
                Services.asSuperUser:
                  s.session.transaction.use { xa =>
                    s.programService
                      .insertCalibrationProgram(
                        ProgramPropertiesInput.Create.Default.some,
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

  test("program selection via PI email") {
    createProgramAs(piCharles).replicateA(2).flatMap { pids =>
      // Add a program where Charles Guiteau is the COI.  It
      // shouldn't match the `WHERE` filter below.
      createProgramAs(piLeon).flatMap { pid =>
        addProgramUserAs(piLeon, pid, partnerLink = PartnerLink.HasPartner(Partner.BR)).flatMap { mid =>
          linkUserAs(piLeon, mid, piCharles.id)
        }
      } >>
      expect(
        user = staff,
        query = s"""
          query {
            programs(
              WHERE: {
                pi: {
                  user: {
                    profile: {
                      email: { EQ: "charles@guiteau.com" }
                    }
                  }
                }
              }
            ) {
              matches { id }
            }
          }
        """,
        expected =
          Json.obj(
            "programs" -> Json.obj(
              "matches" -> Json.fromValues(
                  pids.map { id =>
                    Json.obj("id" -> id.asJson)
                  }
              )
            )
          ).asRight
      )
    }
  }

  test("program selection via user type") {
    createProgramAs(guest1).flatMap { pid1 =>
      createProgramAs(guest2).flatMap { pid2 =>
        expect(
          user = staff,
          query = s"""
            query {
              programs(
                WHERE: {
                  pi: {
                    user: { type: { EQ: GUEST } }
                  }
                }
              ) {
                matches { id }
              }
            }
          """,
          expected =
            Json.obj(
              "programs" -> Json.obj(
                "matches" -> Json.fromValues(
                    List(pid1, pid2).map { id =>
                      Json.obj("id" -> id.asJson)
                    }
                )
              )
            ).asRight
        )
      }
    }
  }

  test("program selection via partner (empty)") {
    expect(
      user = staff,
      query = s"""
        query {
          programs(
            WHERE: {
              pi: {
                partnerLink: { partner: { EQ: US } }
              }
            }
          ) {
            matches { id }
          }
        }
      """,
      expected =
        Json.obj(
          "programs" -> Json.obj(
            "matches" -> Json.arr()
          )
        ).asRight
    )
  }

  val start = LocalDate.parse("2025-02-01")
  val end   = LocalDate.parse("2025-07-01")

  test("program selection via active period"):
    createCallForProposalsAs(staff, activeStart = start, activeEnd = end).flatMap { cid =>
      createProgramAs(pi, "Active Period Test").flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        expect(
          user     = pi,
          query    = s"""
            query {
              programs(
                WHERE: {
                  activeStart: { GT: "2025-01-31" }
                  activeEnd: { LT: "2025-07-02" }
                }
              ) {
                matches { id }
              }
            }
          """,
          expected =
            json"""
              {
                "programs": {
                  "matches": [
                    {
                      "id": ${pid.asJson}
                    }
                  ]
                }
              }
            """.asRight
        )
      }
    }

  test("program selection via active period (empty)"):
    createCallForProposalsAs(staff, activeStart = start, activeEnd = end).flatMap { cid =>
      createProgramAs(pi, "Active Period Test").flatMap { pid =>
        addProposal(pi, pid, cid.some) *>
        expect(
          user     = pi,
          query    = s"""
            query {
              programs(
                WHERE: {
                  activeStart: { GT: "2025-02-02" }
                  activeEnd: { LT: "2025-06-30" }
                }
              ) {
                matches { id }
              }
            }
          """,
          expected =
            json"""
              {
                "programs": {
                  "matches": []
                }
              }
            """.asRight
        )
      }
    }

  test("program selection via cfp"):
    for
      cid0 <- createCallForProposalsAs(staff)
      cid1 <- createCallForProposalsAs(staff)
      pid0 <- createProgramAs(pi, "CfP Test 0")
      pid1 <- createProgramAs(pi, "CfP Test 1")
      pid2 <- createProgramAs(pi, "CfP Test 2")
      _    <- addProposal(pi, pid0, cid0.some)
      _    <- addProposal(pi, pid1, cid1.some)
      _    <- addProposal(pi, pid2, cid0.some)
      _    <- expect(
                user     = pi,
                query    = s"""
                  query {
                    programs(
                      WHERE: {
                        proposal: {
                          call: {
                            id: { EQ: "$cid0" }
                          }
                        }
                      }
                    ) {
                      matches { id }
                    }
                  }
                """,
                expected =
                  json"""
                    {
                      "programs": {
                        "matches": [
                          {
                            "id": ${pid0.asJson}
                          },
                          {
                            "id": ${pid2.asJson}
                          }
                        ]
                      }
                    }
                  """.asRight
              )
    yield ()

}
