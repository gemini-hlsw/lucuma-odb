// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence

class updatePrograms extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.CA)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service).toList

  test("edit name") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  name: "new name"
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              hasMore
              programs {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": {
              "hasMore": false,
              "programs": [
                {
                  "id": $pid,
                  "name": "new name"
                }
              ]
            }
          }
          """
        )
      ) >> chronProgramUpdates(pid).map(_.drop(1)).assertEquals(
        List(
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : true,
            "c_new_name"            : "new name",
            "c_operation"           : "UPDATE",
            "c_mod_pts_pi"          : false,
            "c_new_pts_pi"          : null,
            "c_program_id"          : $pid,
            "c_mod_existence"       : false,
            "c_new_existence"       : null,
            "c_mod_pi_user_id"      : false,
            "c_mod_program_id"      : false,
            "c_new_pi_user_id"      : null,
            "c_new_program_id"      : null,
            "c_mod_pi_user_type"    : false,
            "c_new_pi_user_type"    : null,
            "c_mod_pts_execution"   : false,
            "c_mod_pts_uncharged"   : false,
            "c_new_pts_execution"   : null,
            "c_new_pts_uncharged"   : null,
            "c_mod_proposal_status" : false,
            "c_new_proposal_status" : null
          }
          """
        )
      )
    }
  }

  test("edit existence") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  existence: DELETED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
                includeDeleted: true
              }
            ) {
              programs {
                id
                existence
              }
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": {
              "programs": [
                {
                  "id": $pid,
                  "existence": ${Existence.Deleted:Existence}
                }
              ]
            }
          }
          """
        )
      ) >> chronProgramUpdates(pid).map(_.drop(1)).assertEquals(
        List(
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : false,
            "c_new_name"            : null,
            "c_operation"           : "UPDATE",
            "c_mod_pts_pi"          : false,
            "c_new_pts_pi"          : null,
            "c_program_id"          : $pid,
            "c_mod_existence"       : true,
            "c_new_existence"       : "deleted",
            "c_mod_pi_user_id"      : false,
            "c_mod_program_id"      : false,
            "c_new_pi_user_id"      : null,
            "c_new_program_id"      : null,
            "c_mod_pi_user_type"    : false,
            "c_new_pi_user_type"    : null,
            "c_mod_pts_execution"   : false,
            "c_mod_pts_uncharged"   : false,
            "c_new_pts_execution"   : null,
            "c_new_pts_uncharged"   : null,
            "c_mod_proposal_status" : false,
            "c_new_proposal_status" : null
          }
          """
        )
      )
    }
  }

  test("bulk update basic properties") {
    // create a bunch and edit a few of them
    createProgramAs(pi).replicateA(10).flatMap { pids =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  name: "updated"
                }
                WHERE: {
                  id: {
                    IN: [ ${pids.take(3).mkString("\"", "\", \"", "\"")} ]
                  }
                }
              }
            ) {
              programs {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : ${pids(0)},
                    "name" : "updated"
                  },
                  {
                    "id" : ${pids(1)},
                    "name" : "updated"
                  },
                  {
                    "id" : ${pids(2)},
                    "name" : "updated"
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
