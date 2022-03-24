package lucuma.odb.graphql
package mutation

import io.circe.literal._
import lucuma.odb.graphql.OdbSuite
import cats.syntax.all._
import lucuma.core.model.Partner
import lucuma.core.model.User
import cats.effect.IO
import lucuma.core.model.Program

class createProgram extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.Ca)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service).toList

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

  test("guest + standard/pi,ngo,staff,admin user becomes the PI") {
    List(guest, pi, ngo, staff, admin).traverse { u =>
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

trait CreateProgramOps { this: OdbSuite =>

  def createProgramAs(user: User): IO[Program.Id] =
    query(user, "mutation { createProgram(input: { name: null }) { id } }").flatMap { js =>
      js.hcursor
        .downField("createProgram")
        .downField("id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

}