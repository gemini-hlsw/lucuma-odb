// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class observations extends OdbSuite {

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


  def createObservation(user: User, pid: Program.Id): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
            }) {
              observation {
                id
              }
            }
          }        """
    ) map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }


  test("simple observation selection") {
    createProgram(pi).flatMap { pid =>
      createObservation(pi, pid).replicateA(5).flatMap { oids =>
        expect(
          user = pi,
          query = s"""
            query {
              observations() {
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
                "hasMore" -> Json.False,
                "matches" -> Json.fromValues(
                    oids.map { id =>
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

  test("simple observation selection with limit") {
    createProgram(pi2).flatMap { pid =>
      createObservation(pi2, pid).replicateA(5).flatMap { oids =>
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

  test("simple observation selection, including planned time") {
    createProgram(pi2).flatMap { pid =>
      createObservation(pi2, pid).flatMap { oid =>
        expect(
          user = pi2,
          query = s"""
            query {
              observations(WHERE: { id: { EQ: "$oid" }}) {
                hasMore
                matches {
                  id
                  plannedTime {
                    pi {
                      seconds
                    }
                    uncharged {
                      seconds
                    }
                    execution {
                      seconds
                    }
                  }
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
                      "id" : $oid,
                      "plannedTime" : {
                        "pi" : {
                          "seconds" : 0
                        },
                        "uncharged" : {
                          "seconds" : 0
                        },
                        "execution" : {
                          "seconds" : 0
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

  def createObservationWithSpecRequirementsWithoutWavelength(user: User, pid: Program.Id): IO[Observation.Id] =
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

  def createObservationWithSpecRequirementsWithWavelength(user: User, pid: Program.Id): IO[Observation.Id] =
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

  test("select observations with science requirements containing null and non-null wavelengths") {
    createProgram(pi).flatMap { pid =>
      (createObservationWithSpecRequirementsWithWavelength(pi, pid), createObservationWithSpecRequirementsWithoutWavelength(pi, pid))
        .tupled
        .flatMap { (oid1, oid2) => 
          expect(
            user = pi,
            query = s"""
              query {
                observations(programId: "$pid") {
                  matches {
                    id
                    scienceRequirements {
                      spectroscopy {
                        wavelength {
                          picometers
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
                          }
                        }
                      }
                    },
                    {
                      "id" : $oid2,
                      "scienceRequirements" : {
                        "spectroscopy" : {
                          "wavelength" : null
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

}
