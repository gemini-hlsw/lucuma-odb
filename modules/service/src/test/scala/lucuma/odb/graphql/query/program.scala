package lucuma.odb.graphql
package query

import io.circe.literal._
import lucuma.odb.graphql.OdbSuite
// import io.circe.Json
import lucuma.core.model.Program
import cats.syntax.all._
import cats.effect.IO
import lucuma.core.model.User

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
            createProgram(input: { name: "$name" }) {
              id
            }
          }
        """
    ) map { json =>
      json.hcursor.downFields("createProgram", "id").require[Program.Id]
    }

  test("any user can read their own programs") {
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
                }
              }
            """,
          expected = Right(
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

  test("guest and standard user can't see each others' programs") {
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

  test("service user can see anyone's programs") {
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


}