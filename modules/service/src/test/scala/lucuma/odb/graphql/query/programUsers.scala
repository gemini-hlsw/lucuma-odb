// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.data.PartnerLink
import lucuma.odb.data.ProgramUserRole

class programUsers extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val guest1  = TestUsers.guest(3)
  val guest2  = TestUsers.guest(4)
  val staff   = TestUsers.Standard.staff(5, 34)

  val piCharles = TestUsers.Standard(
    6,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(6).get),
    primaryEmail = "charles@guiteau.com".some
  )

  val piLeon    = TestUsers.Standard(
    7,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(7).get),
    primaryEmail = "leon@czolgosz.edu".some
  )

  val service = TestUsers.service(10)

  val validUsers = List(pi, pi2, guest1, guest2, staff, piCharles, piLeon, service).toList

  test("simple program user selection") {
    createProgramAs(pi).replicateA(5).flatMap { pids =>
      expect(
        user = pi,
        query = s"""
          query {
            programUsers() {
              hasMore
              matches {
                program { id }
                user { id }
              }
            }
          }
        """,
        expected =
          Right(Json.obj(
            "programUsers" -> Json.obj(
              "hasMore" -> Json.False,
              "matches" -> Json.fromValues(
                  pids.map { id =>
                    Json.obj(
                      "program" -> Json.obj("id" -> id.asJson),
                      "user"    -> Json.obj("id" -> pi.id.asJson)
                    )
                  }
              )
            )
          )
        )
      )
    }
  }

  test("program user selection via PI email") {
    createProgramAs(piLeon) >>
    createProgramAs(piCharles).replicateA(2).flatMap { pids =>
      expect(
        user = staff,
        query = s"""
          query {
            programUsers(
              WHERE: {
                user: {
                  orcidEmail: { EQ: "charles@guiteau.com" }
                }
              }
            ) {
              matches {
                program { id }
                user { id }
              }
            }
          }
        """,
        expected =
          Json.obj(
            "programUsers" -> Json.obj(
              "matches" -> Json.fromValues(
                  pids.map { id =>
                    Json.obj(
                      "program" -> Json.obj("id" -> id.asJson),
                      "user"    -> Json.obj("id" -> piCharles.id.asJson)
                    )
                  }
              )
            )
          ).asRight
      )
    }
  }

  test("program user selection via partner") {
    for {
      pid <- createProgramAs(pi2)
      _   <- linkCoiAs(pi2, piCharles.id, pid, Partner.CA)
      _   <- linkCoiAs(pi2, piLeon.id,    pid, Partner.UH)
      _   <- linkAs(pi2, pi.id, pid, ProgramUserRole.Coi, PartnerLink.HasNonPartner)
      _   <- expect(
              user = staff,
              query = s"""
                query {
                  programUsers(
                    WHERE: {
                      partnerLink: { partner: { EQ: UH } }
                    }
                  ) {
                    matches {
                      program { id }
                      user { id }
                    }
                  }
                }
              """,
              expected =
                Json.obj(
                  "programUsers" -> Json.obj(
                    "matches" -> Json.arr(
                      Json.obj(
                        "program" -> Json.obj("id" -> pid.asJson),
                        "user"    -> Json.obj("id" -> piLeon.id.asJson)
                      )
                    )
                  )
                ).asRight
            )
    } yield ()
  }

  test("program user selection limited by visibility") {
    createProgramAs(guest1).flatMap { pid =>
      expect(
        user = guest2,
        query = s"""
          query {
            programUsers() {
              matches {
                program { id }
                user { id }
              }
            }
          }
        """,
        expected =
          Json.obj(
            "programUsers" -> Json.obj(
              "matches" -> Json.arr()
            )
          ).asRight
      )
    }
  }

}
