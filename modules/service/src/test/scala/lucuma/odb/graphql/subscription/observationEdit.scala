// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.User
import lucuma.odb.data.EditType

class observationEdit extends OdbSuite with SubscriptionUtils {

  object Group1 {
    val pi       = TestUsers.Standard.pi(11, 110)
    val guest    = TestUsers.guest(12)
    val service  = TestUsers.service(13)
  }

  object Group2 {
    val pi       = TestUsers.Standard.pi(21, 210)
    val guest    = TestUsers.guest(22)
    val service  = TestUsers.service(23)
  }

  override def validUsers: List[User] =
    List(
      Group1.pi, Group1.guest, Group1.service,
      Group2.pi, Group2.guest, Group2.service
    )

  val subtitleSubscriptionQuery: String =
    """
      subscription {
        observationEdit {
          editType
          value {
            subtitle
          }
        }
      }
    """

  def observationEdit(
    editType: EditType,
    subtitle: String
  ): Json =
    Json.obj(
      "observationEdit" -> Json.obj(
        "editType" -> Json.fromString(editType.tag.toUpperCase),
        "value"    -> Json.obj(
          "subtitle" -> Json.fromString(subtitle)
        )
      )
    )

  def created(subtitle: String): Json =
    observationEdit(EditType.Created, subtitle)

  def updated(subtitle: String): Json =
    observationEdit(EditType.Updated, subtitle)

  test("trigger for my own new observations") {
    import Group1._
    List(pi, guest, service).traverse { user =>
      subscriptionExpect(
        user      = user,
        query     = subtitleSubscriptionQuery,
        mutations =
          Right(
            createProgram(user, "foo").flatMap { pid =>
              createObservation(user, "foo subtitle 0", pid) >> createObservation(user, "foo subtitle 1", pid)
            }
          ),
        expected  = List(created("foo subtitle 0"), created("foo subtitle 1"))
      )
    }
  }

  test("trigger for my own new observations (but nobody else's) as guest user") {
    import Group2._
    subscriptionExpect(
      user      = guest,
      query     = subtitleSubscriptionQuery,
      mutations =
        Right(
          createProgram(guest,   "foo").flatMap(createObservation(guest,   "foo subtitle", _)) >>
          createProgram(pi,      "bar").flatMap(createObservation(pi,      "bar subtitle", _)) >>
          createProgram(service, "baz").flatMap(createObservation(service, "baz subtitle", _))
        ),
      expected  = List(created("foo subtitle"))
    )
  }

  test("trigger for all observations as service user") {
    import Group2._
    subscriptionExpect(
      user      = service,
      query     = subtitleSubscriptionQuery,
      mutations =
        Right(
          createProgram(guest,   "foo").flatMap(createObservation(guest,   "foo subtitle", _)) >>
          createProgram(pi,      "bar").flatMap(createObservation(pi,      "bar subtitle", _)) >>
          createProgram(service, "baz").flatMap(createObservation(service, "baz subtitle", _))
        ),
      expected  = List(created("foo subtitle"), created("bar subtitle"), created("baz subtitle"))
    )
  }

}
