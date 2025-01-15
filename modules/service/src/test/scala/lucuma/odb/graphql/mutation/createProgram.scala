// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.User
import lucuma.odb.graphql.input.ProgramPropertiesInput

class createProgram extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.CA)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service)

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

  test("'description' may be set") {
    expect(
      user = pi,
      query =
        s"""
          mutation {
            createProgram(
              input: {
                SET: {
                  description: "my abstract"
                }
              }
            ) {
              program {
                description
              }
            }
          }
        """,
      expected = Right(
        json"""
          {
            "createProgram": {
              "program": {
                "description": "my abstract"
              }
            }
          }
        """
      )
    )
  }

  test("GOA properties default") {
    expect(
      user = pi,
      query =
        """
          mutation {
            createProgram(
              input: {
                SET: {
                  name: "Foo"
                }
              }
            ) {
              program {
                name
                goa {
                  proprietaryMonths
                  shouldNotify
                  privateHeader
                }
              }
            }
          }
        """,
      expected =
        json"""
          {
            "createProgram" : {
              "program": {
                "name" : "Foo",
                "goa": {
                  "proprietaryMonths": 0,
                  "shouldNotify": true,
                  "privateHeader": false
                }
              }
            }
          }
        """.asRight
    )
  }

  val CreateWithProprietaryMonths =
    """
      mutation {
        createProgram(
          input: {
            SET: {
              name: "Foo",
              goa: { proprietaryMonths: 42 }
            }
          }
        ) {
          program {
            name
            goa { proprietaryMonths }
          }
        }
      }
    """

  test("proprietaryMonths may be specified as staff") {
    expect(
      user     = staff,
      query    = CreateWithProprietaryMonths,
      expected =
        json"""
          {
            "createProgram" : {
              "program": {
                "name" : "Foo",
                "goa": { "proprietaryMonths": 42 }
              }
            }
          }
        """.asRight
    )
  }

  test("proprietaryMonths may not be specified as pi") {
    expect(
      user     = pi,
      query    = CreateWithProprietaryMonths,
      expected = List(
        "Only staff may set the proprietary months."
      ).asLeft
    )
  }

  test("pi can set other GOA properties") {
    expect(
      user     = pi,
      query    = """
        mutation {
          createProgram(
            input: {
              SET: {
                name: "Foo",
                goa: {
                  shouldNotify: false,
                  privateHeader: true
                }
              }
            }
          ) {
            program {
              name
              goa {
                shouldNotify
                privateHeader
              }
            }
          }
        }
      """,
      expected =
        json"""
          {
            "createProgram" : {
              "program": {
                "name" : "Foo",
                "goa": {
                  "shouldNotify": false,
                  "privateHeader": true
                }
              }
            }
          }
        """.asRight
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
                  pi { user { id } }
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
                    "user": {
                      "id" : ${u.id}
                    }
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
                pi { user { id } }
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

  test("service user can create a calibration program") {
    val name = "Photometric calibration targeets"
    for {
      pid <- withServices(service) { s =>
              s.session.transaction.use { xa =>
                s.programService
                  .insertCalibrationProgram(
                    ProgramPropertiesInput.Create.Default.copy(
                      name = NonEmptyString.from(name).toOption
                    ).some,
                    CalibrationRole.Photometric,
                    Description.unsafeFrom("PHOTO"))(using xa)
              }
            }
      _ <- expect(
            user = staff,
            query =
              s"""
                query {
                  program(programId: "$pid") {
                    id
                    name
                    calibrationRole
                    pi { user { id } }
                  }
                }
              """,
            expected = Right(
              json"""
                {
                  "program": {
                    "id": $pid,
                    "name": $name,
                    "calibrationRole": "PHOTOMETRIC",
                    "pi": null
                  }
                }
              """
            )
          )
    } yield ()
  }

  test("chronicle auditing") {
    createProgramAs(pi, "Foo").flatMap { pid =>
      assertIO(chronProgramUpdates(pid), List(
        json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : true,
            "c_new_name"            : "Foo",
            "c_operation"           : "INSERT",
            "c_mod_pts_pi"          : true,
            "c_new_pts_pi"          : "PT0S",
            "c_program_id"          : ${pid},
            "c_mod_existence"       : true,
            "c_new_existence"       : "present",
            "c_mod_program_id"      : true,
            "c_new_program_id"      : ${pid},
            "c_mod_pts_execution"   : true,
            "c_mod_pts_uncharged"   : true,
            "c_new_pts_execution"   : "PT0S",
            "c_new_pts_uncharged"   : "PT0S",
            "c_mod_proposal_status" : true,
            "c_new_proposal_status" : "not_submitted"
          }
        """
      ))
    }
  }

}
