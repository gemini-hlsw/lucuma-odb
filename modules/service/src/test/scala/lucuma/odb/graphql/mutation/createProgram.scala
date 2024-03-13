// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User

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

  test("proposalStatus is disallowed") {
    expect(
      user = pi,
      query =
        """
          mutation {
            createProgram(
              input: {
                SET: {
                  name: "Program name",
                  proposalStatus: NOT_SUBMITTED
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
            "Argument 'input.SET' is invalid: proposalStatus cannot be specified during program creation."
          )
        ),
    )
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
            "c_mod_pi_user_id"      : true,
            "c_mod_program_id"      : true,
            "c_new_pi_user_id"      : "u-1",
            "c_new_program_id"      : ${pid},
            "c_mod_pi_user_type"    : true,
            "c_new_pi_user_type"    : "standard",
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
