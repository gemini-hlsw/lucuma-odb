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

  test("pi can select their own target") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
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
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
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
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
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