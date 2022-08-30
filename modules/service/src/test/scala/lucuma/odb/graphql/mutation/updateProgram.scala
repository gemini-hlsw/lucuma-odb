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
import lucuma.odb.data.Existence
import lucuma.core.math.Offset.P

class updateProgram extends OdbSuite with CreateProgramOps {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.Ca)
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
              id
              name
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": [
              {
                "id": $pid,
                "name": "new name"
              }
            ]
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
              id
              existence
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": [
              {
                "id": $pid,
                "existence": ${Existence.Deleted:Existence}
              }
            ]
          }
          """
        )
      )
    }
  }

}