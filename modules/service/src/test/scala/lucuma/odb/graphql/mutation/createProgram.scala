package lucuma.odb.graphql
package mutation

import io.circe.literal._
import lucuma.odb.graphql.OdbSuite
import cats.syntax.all._

class createProgram extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)

  val validUsers = List(pi, guest, service).toList

  test("empty 'name' is disallowed") {
    expect(
      user = pi,
      query =
        """
          mutation {
            createProgram(input: { name: "" }) {
              id
            }
          }
        """,
      expected =
        Left(
          List(
            "Argument 'name' is invalid: string value must be non-empty."
          )
        ),
    )
  }

  test("null 'name' is ok") {
    expect(
      user = pi,
      query =
        s"""
          mutation {
            createProgram(input: { name: null }) {
              name
            }
          }
        """,
      expected = Right(
        json"""
          {
            "createProgram": {
              "name": null
            }
          }
        """
      )
    )
  }

  test("guest/pi user becomes the PI") {
    List(guest, pi).traverse { u =>
      val name = s"${u.displayName}'s Science Program"
      expect(
        user   = u,
        query  =
          s"""
            mutation {
              createProgram(input: { name: "$name" }) {
                name
                pi {
                  id
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
              "createProgram" : {
                "name" : $name,
                "pi" : {
                    "id" : ${u.id}
                }
              }
            }
          """
        ),
      )
    }
  }

  test("service user does not become the PI") {
    val name = s"${service.displayName}'s Science Program"
    expect(
      user   = service,
      query  =
        s"""
          mutation {
            createProgram(input: { name: "$name" }) {
              name
              pi {
                id
              }
            }
          }
        """,
      expected = Right(
        json"""
          {
            "createProgram" : {
              "name" : $name,
              "pi" : null
            }
          }
        """
      ),
    )
  }

}