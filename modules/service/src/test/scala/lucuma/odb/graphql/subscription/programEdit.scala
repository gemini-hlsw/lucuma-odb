// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite
import munit.IgnoreSuite

import scala.concurrent.duration._

// N.B. this works locally, most of the time. Need to get it working reliably.
@IgnoreSuite
class programEdit extends OdbSuite {

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

  def createProgram(user: User, name: String): IO[Program.Id] =
    IO.sleep(500.millis) >> // try to behave nicely on weak CI machines
    query(
      user = user,
      query =
        s"""
          mutation {
            createProgram(input: { SET: { name: "$name" } }) {
              program {
                id
              }
            }
          }
        """
    ) map { json =>
      json.hcursor.downFields("createProgram", "program", "id").require[Program.Id]
    }

  test("trigger for my own new programs") {
    import Group1._
    List(pi, guest, service).traverse { user =>
      subscriptionTest(
        user = user,
        query =
          """
            subscription {
              programEdit {
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
            json"""{ "programEdit": { "value": { "name": "foo" } } }""",
            json"""{ "programEdit": { "value": { "name": "bar" } } }""",
          )
      )
    }
  }

  test("trigger for my own new programs (but nobody else's) as guest user") {
    import Group2._
    subscriptionTest(
      user = guest,
      query =
        """
          subscription {
            programEdit {
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
          json"""{ "programEdit": { "value": { "name": "foo" } } }""",
        )
    )
  }

  test("trigger for my own new programs (but nobody else's) as standard user in PI role") {
    import Group2._
    subscriptionTest(
      user = pi,
      query =
        """
          subscription {
            programEdit {
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
          json"""{ "programEdit": { "value": { "name": "bar" } } }""",
        )
    )
  }

  test("trigger for all programs as service user") {
    import Group2._
    subscriptionTest(
      user = service,
      query =
        """
          subscription {
            programEdit {
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
          json"""{ "programEdit": { "value": { "name": "foo" } } }""",
          json"""{ "programEdit": { "value": { "name": "bar" } } }""",
          json"""{ "programEdit": { "value": { "name": "baz" } } }""",
        )
    )
  }

}