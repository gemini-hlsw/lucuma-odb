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
// @IgnoreSuite
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

  test("event ids should be distinct") {
    import Group2._
    subscription(
      user = service,
      query = // N.B. if we don't select something from the value we'll hit a problem
        """
          subscription {
            programEdit {
              id
              value {
                id
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
    ).map { js =>
      val ids = js.map(_.hcursor.downFields("programEdit", "id").require[Long]).distinct
      assertEquals(ids.length, 3, "Should get three distinct IDs.")
    }
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
                  id
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

}