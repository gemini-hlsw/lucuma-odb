// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

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
            createProgram(
              input: {
                SET: {
                  name: ""
                }
              }
            ) {
              program {
                id
              }
            }
          }
        """,
      expected =
        Left(
          List(
            "Argument 'input.SET.name' is invalid: string value must be non-empty."
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
            createProgram(
              input: {
                SET: {
                  name: null
                }
              }
            ) {
              program {
                name
              }
            }
          }
        """,
      expected = Right(
        json"""
          {
            "createProgram": {
              "program": {
                "name": null
              }
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
              createProgram(
                input: {
                  SET: {
                    name: "$name"
                  }
                }
              ) {
                program {
                  name
                  pi {
                    id
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
              "createProgram" : {
                "program": {
                  "name" : $name,
                  "pi" : {
                      "id" : ${u.id}
                  }
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
            createProgram(
              input: {
                SET: {
                  name: "$name"
                }
              }
            ) {
              program {
                name
                pi {
                  id
                }
              }
            }
          }
        """,
      expected = Right(
        json"""
          {
            "createProgram" : {
              "program": {
                "name" : $name,
                "pi" : null
              }
            }
          }
        """
      ),
    )
  }

}

trait CreateProgramOps { this: OdbSuite =>

  def createProgramAs(user: User): IO[Program.Id] =
    query(user, "mutation { createProgram(input: { SET: { name: null } }) { program { id } } }").flatMap { js =>
      js.hcursor
        .downField("createProgram")
        .downField("program")
        .downField("id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

}