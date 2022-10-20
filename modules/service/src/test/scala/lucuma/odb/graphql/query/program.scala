// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite
import lucuma.core.model.Observation

class program extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)

  val validUsers = List(pi, guest, service).toList

  def createProgram(user: User, name: String): IO[Program.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createProgram(input: { SET: { name: "$name" } }) {
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

  test("any user can read their own programs".ignore) {
    List(guest, pi, service).traverse { user =>
      val name = s"${user.displayName}'s Science Program"
      createProgram(user, name).flatMap { id =>
        expect(
          user = user,
          query =
            s"""
              query {
                program(programId: "$id") {
                  id
                  name
                  proposal {
                    title
                    category
                    toOActivation
                    abstract
                    partnerSplits {
                      partner
                      percent
                    }
                    proposalClass {
                      minPercentTime
                      ... on LargeProgram {
                        minPercentTotalTime
                        totalTime {
                          iso
                        }
                      }
                      ... on Intensive {
                        minPercentTotalTime
                        totalTime {
                          iso
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
                "program": {
                  "id": $id,
                  "name": $name,
                  "proposal": null
                }
              }
            """
          )
        )
      }
    }
  }

  test("guest and standard user can't see each others' programs".ignore) {
    val users = List(guest, pi)
    users.traverse { user =>
      val name = s"${user.displayName}'s Science Program"
      createProgram(user, name).flatMap { id =>
        users.traverse { user2 =>
          expect(
            user = user2,
            query =
              s"""
                query {
                  program(programId: "$id") {
                    id
                    name
                  }
                }
              """,
            expected =
              if (user == user2) {
                Right(
                  json"""
                    {
                      "program": {
                        "id": $id,
                        "name": $name
                      }
                    }
                  """
                )
              } else {
                Right(
                  json"""
                    {
                      "program": null
                    }
                  """
                )
              }
          )
        }
      }
    }
  }

  test("service user can see anyone's programs".ignore) {
    val users = List(guest, pi)
    users.traverse { user =>
      val name = s"${user.displayName}'s Science Program"
      createProgram(user, name).flatMap { id =>
        expect(
          user = service,
          query =
            s"""
              query {
                program(programId: "$id") {
                  id
                  name
                }
              }
            """,
          expected =
            Right(
              json"""
                {
                  "program": {
                    "id": $id,
                    "name": $name
                  }
                }
              """
            )
        )
      }
    }
  }

  test("program / observations (simple)") {
    createProgram(pi, "program with some observations").flatMap { pid =>
      createObservation(pi, pid).replicateA(3).flatMap { oids =>
        expect(
          user = pi,
          query = 
             s"""
              query {
                program(programId: "$pid") {
                  id
                  observations() {
                    matches {
                      id
                    }
                  }
                }
              }
            """,
          expected =
            Right(
              json"""
                {
                  "program": {
                    "id": $pid
                  }
                }
              """
            )
        )      
      }
    }
  }

}