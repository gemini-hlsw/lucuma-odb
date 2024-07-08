// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.ScienceBand

class observations extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service).toList

  test("simple observation selection") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid).replicateA(5).flatMap { oids =>
        expect(
          user = pi,
          query = s"""
            query {
              observations() {
                hasMore
                matches {
                  id
                  calibrationRole
                  observerNotes
                }
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

  test("simple observation selection with limit") {
    createProgramAs(pi2).flatMap { pid =>
      createObservationAs(pi2, pid).replicateA(5).flatMap { oids =>
        expect(
          user = pi2,
          query = s"""
            query {
              observations(LIMIT: 3) {
                hasMore
                matches {
                  id
                }
              }
            }
          """,
          expected =
            Right(Json.obj(
              "observations" -> Json.obj(
                "hasMore" -> Json.True,
                "matches" -> Json.fromValues(
                    oids.take(3).map { id =>
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

  test("simple observation selection") {
    createProgramAs(pi2).flatMap { pid =>
      createObservationAs(pi2, pid).flatMap { oid =>
        expect(
          user = pi2,
          query = s"""
            query {
              observations(WHERE: { id: { EQ: "$oid" }}) {
                hasMore
                matches {
                  id
                }
              }
            }
          """,
        expected =
          Right(
            json"""
              {
                "observations" : {
                  "hasMore" : false,
                  "matches" : [
                    {
                      "id" : $oid
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("select science bands") {
    for {
      pid  <- createProgramAs(pi2)
      oid1 <- createObservationAs(pi2, pid)
      _    <- setScienceBandAs(pi2, oid1, ScienceBand.Band1.some)
      oid2 <- createObservationAs(pi2, pid)
      _    <- setScienceBandAs(pi2, oid2, ScienceBand.Band1.some)
      oid3 <- createObservationAs(pi2, pid)
      _    <- setScienceBandAs(pi2, oid3, ScienceBand.Band2.some)
      oid4 <- createObservationAs(pi2, pid)
      b1   <- observationsWhere(pi2, """scienceBand: { EQ: BAND1 }""")
      b2   <- observationsWhere(pi2, """scienceBand: { EQ: BAND2 }""")
      b3   <- observationsWhere(pi2, """scienceBand: { EQ: BAND3 }""")
      bn   <- observationsWhere(pi2, s"""program: { id: { EQ: "$pid" } }, scienceBand: { IS_NULL: true }""")
    } yield {
      assertEquals(b1, List(oid1, oid2))
      assertEquals(b2, List(oid3))
      assertEquals(b3, Nil)
      assertEquals(bn, List(oid4))
    }
  }

  def createObservationWithNullSpecRequirements(user: User, pid: Program.Id): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                scienceRequirements: {
                  mode: SPECTROSCOPY
                }
              }
            }) {
              observation {
                id
              }
            }
          }
          """
    ) map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  def createObservationWithDefinedSpecRequirements(user: User, pid: Program.Id): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                scienceRequirements: {
                  mode: SPECTROSCOPY
                  spectroscopy: {
                    wavelength: {
                      angstroms: 42
                    }
                    signalToNoiseAt: {
                      angstroms: 71
                    }
                    wavelengthCoverage: {
                      angstroms: 99
                    }
                    focalPlaneAngle: {
                      arcseconds: 666
                    }
                  }
                }
              }
            }) {
              observation {
                id
              }
            }
          }
          """
    ) map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  test("select observations with science requirements containing null and non-null embeds") {
    createProgramAs(pi).flatMap { pid =>
      (createObservationWithDefinedSpecRequirements(pi, pid), createObservationWithNullSpecRequirements(pi, pid))
        .tupled
        .flatMap { (oid1, oid2) =>
          expect(
            user = pi,
            query = s"""
              query {
                observations(WHERE: {
                  program: {
                    id: { EQ: "$pid" }
                  }
                }) {
                  matches {
                    id
                    scienceRequirements {
                      spectroscopy {
                        wavelength {
                          picometers
                        }
                        signalToNoiseAt {
                          picometers
                        }
                        wavelengthCoverage {
                          picometers
                        }
                        focalPlaneAngle {
                          milliarcseconds
                        }
                      }
                    }
                  }
                }
              }
            """,
            expected = Right(
              json"""
              {
                "observations" : {
                  "matches" : [
                    {
                      "id" : $oid1,
                      "scienceRequirements" : {
                        "spectroscopy" : {
                          "wavelength" : {
                            "picometers" : 4200
                          },
                          "signalToNoiseAt" : {
                            "picometers" : 7100
                          },
                          "wavelengthCoverage" : {
                            "picometers" : 9900
                          },
                          "focalPlaneAngle" : {
                            "milliarcseconds" : 666000
                          }
                        }
                      }
                    },
                    {
                      "id" : $oid2,
                      "scienceRequirements" : {
                        "spectroscopy" : {
                          "wavelength" : null,
                          "signalToNoiseAt" : null,
                          "wavelengthCoverage" : null,
                          "focalPlaneAngle" : null
                        }
                      }
                    }
                  ]
                }
              }
              """
            )
          )
        }
    }
  }

  test("select group info on observation without a group") {
    for {
      pid <- createProgramAs(pi2)
      oid <- createObservationAs(pi2, pid)
      _   <- expect(
               user = pi2,
               query = s"""
              query {
                observations(WHERE: { id: { EQ: "$oid" }}) {
                  hasMore
                  matches {
                    id
                    groupId
                    groupIndex
                  }
                }
              }""",
               expected = Right(json"""
                        {
                          "observations" : {
                            "hasMore" : false,
                            "matches" : [
                              {
                                "id" : $oid,
                                "groupId" : null,
                                "groupIndex" : 0
                              }
                            ]
                          }
                        }""")
             )
    } yield ()
  }

  test("select group info on observation with a group") {
    for {
      pid <- createProgramAs(pi2)
      gid <- createGroupAs(pi2, pid)
      oid <- createObservationInGroupAs(pi2, pid, gid.some)
      _   <- expect(
               user = pi2,
               query = s"""
              query {
                observations(WHERE: { id: { EQ: "$oid" }}) {
                  hasMore
                  matches {
                    id
                    groupId
                    groupIndex
                  }
                }
              }""",
               expected = Right(json"""
                        {
                          "observations" : {
                            "hasMore" : false,
                            "matches" : [
                              {
                                "id" : $oid,
                                "groupId" : $gid,
                                "groupIndex" : 0
                              }
                            ]
                          }
                        }
                      """)
             )

    } yield ()
  }
}
