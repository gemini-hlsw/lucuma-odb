// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class target extends OdbSuite {

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

  def createTarget(user: User, pid: Program.Id): IO[Target.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createTarget(
              input: {
                programId: ${pid.asJson}
                SET: {
                  name: "Crunchy Target"
                  sidereal: {
                    ra: { degrees: "12.345" }
                    dec: { degrees: "45.678" }
                  }
                  sourceProfile: {
                    point: {
                      bandNormalized: {
                        sed: { stellarLibrary: B5_III }
                        brightnesses: []
                      }
                    }
                  }
                }
              }
            ) {
              target { id }
            }
          }
        """
    ) map { json =>
      json.hcursor.downFields("createTarget", "target", "id").require[Target.Id]
    }

  def createUsers(users: User*): IO[Unit] =
    users.toList.traverse_(createProgram) // TODO: something cheaper

  test("pi can select their own target") {
    createProgram(pi).flatMap { pid =>
      createTarget(pi, pid).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            query {
              target(targetId: ${tid.asJson}) {
                id
              }
            }
          """,
          expected = Right(json"""
            {
              "target": {
                "id": $tid
              }
            }
          """)
        )
      }
    }
  }

  test("pi can't select another pi's target") {
    createProgram(pi).flatMap { pid =>
      createTarget(pi, pid).flatMap { tid =>
        createUsers(pi2) >>
        expect(
          user = pi2,
          query = s"""
            query {
              target(targetId: ${tid.asJson}) {
                id
              }
            }
          """,
          expected = Right(json"""
            {
              "target": null
            }
          """)
        )
      }
    }
  }

  test("service user can select anyone's target") {
    createProgram(pi).flatMap { pid =>
      createTarget(pi, pid).flatMap { tid =>
        createUsers(service) >>
        expect(
          user = service,
          query = s"""
            query {
              target(targetId: ${tid.asJson}) {
                id
              }
            }
          """,
          expected = Right(json"""
            {
              "target": {
                "id": $tid
              }
            }
          """)
        )
      }
    }
  }
}