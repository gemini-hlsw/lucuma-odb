// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.syntax.traverse.*
import io.circe.literal.*
import lucuma.core.model.User

class observationEdit extends OdbSuite with SubscriptionUtils {

  object Group1 {
    val pi       = TestUsers.Standard.pi(11, 110)
    val guest    = TestUsers.guest(12)
    val service  = TestUsers.service(13)
  }

  override def validUsers: List[User] =
    List(
      Group1.pi, Group1.guest, Group1.service
    )

  test("trigger for my own new observations") {
    import Group1._
    List(pi, guest, service).traverse { user =>
      subscriptionExpect(
        user  = user,
        query =
          """
            subscription {
              observationEdit {
                editType
                value {
                  subtitle
                }
              }
            }
          """,
        mutations =
          Right(
            createProgram(user, "foo").flatMap { pid =>
              createObservation(user, "foo subtitle 0", pid) >> createObservation(user, "foo subtitle 1", pid)
            }
          ),
        expected =
          List(
            json"""{ "observationEdit":  { "editType":  "CREATED", "value":  { "subtitle":  "foo subtitle 0" } } }""",
            json"""{ "observationEdit":  { "editType":  "CREATED", "value":  { "subtitle":  "foo subtitle 1" } } }"""
          )
      )
    }
  }
}
