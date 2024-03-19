// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.model.User

import scala.concurrent.duration.*

// N.B. this works locally, most of the time. Need to get it working reliably.
// @IgnoreSuite
class programEdit extends OdbSuite with SubscriptionUtils {

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

  def validUsers =
    List(
      Group1.pi, Group1.guest, Group1.service,
      Group2.pi, Group2.guest, Group2.service,
    )

  test("trigger for my own new programs") {
    import Group1._
    List(pi, guest, service).traverse { user =>
      subscriptionExpect(
        user = user,
        query =
          """
            subscription {
              programEdit {
                editType
                value {
                  name
                }
              }
            }
          """,
        mutations =
          Right(
            createProgram(user, "foo") >>
            createProgram(user, "bar")
          ),
        expected =
          List(
            json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo" } } }""",
            json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "bar" } } }""",
          )
      )
    }
  }

  test("trigger for my own new programs (but nobody else's) as guest user") {
    import Group2._
    subscriptionExpect(
      user = guest,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(guest, "foo") >>
          createProgram(pi, "bar") >>
          createProgram(service, "baz")
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo" } } }""",
        )
    )
  }

  test("trigger for my own new programs (but nobody else's) as standard user in PI role") {
    import Group2._
    subscriptionExpect(
      user = pi,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(guest, "foo") >>
          createProgram(pi, "bar") >>
          createProgram(service, "baz")
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "bar" } } }""",
        )
    )
  }

  test("trigger for all programs as service user") {
    import Group2._
    subscriptionExpect(
      user = service,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(guest, "foo") >>
          createProgram(pi, "bar") >>
          createProgram(service, "baz")
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo" } } }""",
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "bar" } } }""",
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "baz" } } }""",
        )
    )
  }

  test("edit event should show up") {
    import Group2._
    subscriptionExpect(
      user = service,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(guest, "foo").flatMap { id =>
            IO.sleep(1.second) >> // give time to see the creation before we do an update
            query(
              service,
              s"""
              mutation {
                updatePrograms(input: {
                  WHERE: { id: { EQ: "$id" } }
                  SET: { name: "foo2" }
                }) {
                  programs {
                    id
                  }
                }
              }
              """
            )
          } >>
          createProgram(service, "baz").void
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo" } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo2" } } }""",
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "baz" } } }""",
        )
    )
  }

  test("work even if no database fields are selected") {
    import Group1.pi
    subscriptionExpect(
      user      = pi,
      query     = s"""
        subscription {
          programEdit {
            editType
            id
          }
        }
      """,
      mutations =
        Right(
          createProgramAs(pi).replicateA(2)
        ),
      expected = List.fill(2)(json"""{"programEdit":{"editType":"CREATED","id":0}}""")
    )
  }

}
