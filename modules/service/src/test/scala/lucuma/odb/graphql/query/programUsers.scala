// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.core.model.UserProfile
import lucuma.core.util.Gid

class programUsers extends OdbSuite:

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

  val piPhd    = TestUsers.Standard(
    8,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(8).get),
    email = "leon@czolgosz.edu".some
  )

  val pi3     = TestUsers.Standard.pi(9, 35)

  val service = TestUsers.service(10)

  val piJohn  = TestUsers.Standard(
    11,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(11).get),
    familyName = "Booth".some,
    email = "john@wilkes.net".some
  )

  val validUsers = List(pi, pi2, pi3, guest1, guest2, staff, piCharles, piLeon, piPhd, service, piJohn).toList

  test("simple program user selection"):
    createProgramAs(pi).replicateA(5).flatMap: pids =>
      expect(
        user = pi,
        query = s"""
          query {
            programUsers() {
              hasMore
              matches {
                program { id }
                user { id }
                educationalStatus
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
                      "program"           -> Json.obj("id" -> id.asJson),
                      "user"              -> Json.obj("id" -> pi.id.asJson),
                      "educationalStatus" -> Json.Null
                    )
                  }
              )
            )
          )
        )
      )

  test("program user selection via id"):
    val lho = UserProfile("Lee".some, "Oswald".some, "Lee Harvey Oswald".some, "lee@oswald.co".some)
    createProgramAs(piJohn).flatMap: pid =>
      addProgramUserAs(piJohn, pid, preferred = lho).flatMap: puid =>
        expect(
          user = staff,
          query = s"""
            query {
              programUsers(
                WHERE: {
                  id: { EQ: "$puid" }
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
                "matches"    -> Json.fromValues(List(
                   Json.obj(
                     "program" -> Json.obj("id" -> pid.asJson),
                     "user"    -> Json.Null
                   )
                ))
              )
            ).asRight
        )

  test("program user selection via PI email"):
    createProgramAs(piLeon) >>
    createProgramAs(piCharles).replicateA(2).flatMap: pids =>
      expect(
        user = staff,
        query = s"""
          query {
            programUsers(
              WHERE: {
                user: {
                  profile: {
                    email: { EQ: "charles@guiteau.com" }
                  }
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
              "matches"    -> Json.fromValues(
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

  test("program user selection via educational status"):
    createProgramAs(piPhd).replicateA(2).flatMap: pids =>
      expect(
        user = piPhd,
        query = s"""
          query {
            programUsers(
              WHERE: {
                educationalStatus: {
                  IS_NULL: true
                }
              }
            ) {
              matches {
                program { id }
                user { id }
                educationalStatus
                thesis
              }
            }
          }
        """,
        expected =
          Json.obj(
            "programUsers" -> Json.obj(
              "matches"    -> Json.fromValues(
                  pids.map { id =>
                    Json.obj(
                      "program"           -> Json.obj("id" -> id.asJson),
                      "user"              -> Json.obj("id" -> piPhd.id.asJson),
                      "educationalStatus" -> Json.Null,
                      "thesis"            -> Json.Null
                    )
                  }
              )
            )
          ).asRight
      )

  test("program user selection via thesis"):
    createProgramAs(pi3).replicateA(2).flatMap: pids =>
      expect(
        user = pi3,
        query = s"""
          query {
            programUsers(
              WHERE: {
                thesis: {
                  IS_NULL: true
                }
              }
            ) {
              matches {
                program { id }
                user { id }
                thesis
              }
            }
          }
        """,
        expected =
          Json.obj(
            "programUsers" -> Json.obj(
              "matches"    -> Json.fromValues(
                  pids.map { id =>
                    Json.obj(
                      "program" -> Json.obj("id" -> id.asJson),
                      "user"    -> Json.obj("id" -> pi3.id.asJson),
                      "thesis"  -> Json.Null
                    )
                  }
              )
            )
          ).asRight
      )

  test("program user selection via partner"):
    for
      pid  <- createProgramAs(pi2)
      rid0 <- addProgramUserAs(pi2, pid, partnerLink = PartnerLink.HasPartner(Partner.CA))
      rid1 <- addProgramUserAs(pi2, pid, partnerLink = PartnerLink.HasPartner(Partner.UH))
      rid2 <- addProgramUserAs(pi2, pid, partnerLink = PartnerLink.HasNonPartner)
      _    <- linkUserAs(pi2, rid0, piCharles.id)
      _    <- linkUserAs(pi2, rid1, piLeon.id)
      _    <- linkUserAs(pi2, rid2, pi.id)
      _    <- expect(
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
    yield ()

  test("program user selection limited by visibility"):
    createProgramAs(guest1).flatMap: _ =>
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

  test("program user selection unlinked / linked"):
    def queryString(pid: Program.Id, isNull: Boolean): String =
      s"""
        query {
          programUsers(
            WHERE: {
              program: { id : { EQ: "$pid" } }
              user: { IS_NULL: $isNull }
            }
          ) {
            matches {
              id
              preferredProfile { givenName }
            }
          }
        }
      """

    val cg  = UserProfile("Charles".some, "Guiteau".some, none, none)
    val jwb = UserProfile("John".some, "Booth".some, none, none)
    val lc  = UserProfile("Leon".some, "Czolgosz".some, none, none)

    for
      pid  <- createProgramAs(pi2)
      rid0 <- addProgramUserAs(pi2, pid, partnerLink = PartnerLink.HasPartner(Partner.CA), preferred = cg)
      rid1 <- addProgramUserAs(pi2, pid, partnerLink = PartnerLink.HasPartner(Partner.UH), preferred = jwb)
      rid2 <- addProgramUserAs(pi2, pid, partnerLink = PartnerLink.HasNonPartner, preferred = lc)
      _    <- linkUserAs(pi2, rid0, piCharles.id)
      _    <- linkUserAs(pi2, rid2, piLeon.id)
      _    <- expect(
               user     = staff,
               query    = queryString(pid, true),
               expected =
                 json"""
                   {
                     "programUsers": {
                       "matches": [
                         {
                           "id": $rid1,
                           "preferredProfile": { "givenName":  "John" }
                         }
                       ]
                     }
                   }
                 """.asRight
             )
      rid3 <- piProgramUserIdAs(pi2, pid)
      _    <- expect(
               user     = staff,
               query    = queryString(pid, false),
               expected =
                 json"""
                   {
                     "programUsers": {
                       "matches": [
                         {
                           "id": $rid3,
                           "preferredProfile": { "givenName": null }
                         },
                         {
                           "id": $rid0,
                           "preferredProfile": { "givenName":  "Charles" }
                         },
                         {
                           "id": $rid2,
                           "preferredProfile": { "givenName":  "Leon" }
                         }
                       ]
                     }
                   }
                 """.asRight
             )
    yield ()

  test("diplay name"):
    expect(
      user = staff,
      query = s"""
        query {
          programUsers(
              WHERE: {
                user: { id: { EQ: "${piJohn.id}" } }
              }
          ) {
            matches {
              displayName
            }
          }
        }
      """,
      expected =
        json"""
          {
            "programUsers": {
              "matches": [
                {
                  "displayName": "Booth"
                }
              ]
            }
          }
        """.asRight
      )