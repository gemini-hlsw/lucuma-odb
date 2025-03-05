// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence

class createProgramNote extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)

  val validUsers = List(pi, staff)

  def createNoteAs(
    user:      User,
    pid:       Program.Id,
    title:     String,
    text:      Option[String]    = none,
    isPrivate: Option[Boolean]   = none,
    existence: Option[Existence] = none
  ): IO[Unit] =
    val props = List(
      s"""title: "$title"""".some,
      text.map(t => s"""text: "$t""""),
      isPrivate.map(b => s"isPrivate: $b"),
      existence.map(e => s"""existence: "${e.tag.toUpperCase}" """)
    ).flatten.mkString("{\n", "\n", "}\n")

    expect(
      user  = user,
      query = s"""
        mutation {
          createProgramNote(
            input: {
              programId: "$pid"
              SET: $props
            }
          ) {
            programNote {
              title
              text
              isPrivate
              existence
            }
          }
        }
      """,
      expected = json"""
        {
          "createProgramNote": {
            "programNote": {
              "title": ${title.asJson},
              "text": ${text.getOrElse("").asJson},
              "isPrivate": ${isPrivate.getOrElse(false).asJson},
              "existence": ${existence.getOrElse(Existence.Present).asJson}
            }
          }
        }
      """.asRight
    )

  test("one note"):
    createProgramAs(pi).flatMap: pid =>
      createNoteAs(pi, pid, "Foo", "Bar".some) *>
      expect(
        user  = pi,
        query = s"""
          query {
            program(programId: "$pid") {
              notes {
                title
                text
              }
            }
          }
        """,
        expected = json"""
          {
            "program": {
              "notes": [
                {
                  "title": "Foo",
                  "text": "Bar"
                }
              ]
            }
          }
        """.asRight
      )

  test("two notes"):
    createProgramAs(pi).flatMap: pid =>
      createNoteAs(pi, pid, "Foo", "Bar".some) *>
      createNoteAs(pi, pid, "Baz", "Buz".some) *>
      expect(
        user  = pi,
        query = s"""
          query {
            program(programId: "$pid") {
              notes {
                title
                text
              }
            }
          }
        """,
        expected = json"""
          {
            "program": {
              "notes": [
                {
                  "title": "Foo",
                  "text": "Bar"
                },
                {
                  "title": "Baz",
                  "text": "Buz"
                }
              ]
            }
          }
        """.asRight
      )


  test("missing title is disallowed"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = pi,
        query = s"""
          mutation {
            createProgramNote(
              input: {
                programId: "$pid"
                SET: {
                  text: "Foo"
                }
              }
            ) {
              programNote { title }
            }
          }
        """,
        expected = List(
          "Argument 'input.SET.title' is invalid: String is not optional"
        ).asLeft
      )

  test("empty title is disallowed"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = pi,
        query = s"""
          mutation {
            createProgramNote(
              input: {
                programId: "$pid"
                SET: {
                  title: ""
                  text: "Foo"
                }
              }
            ) {
              programNote { title }
            }
          }
        """,
        expected = List(
          "Argument 'input.SET.title' is invalid: string value must be non-empty."
        ).asLeft
      )

  test("pi cannot create a private note"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = pi,
        query = s"""
          mutation {
            createProgramNote(
              input: {
                programId: "$pid"
                SET: {
                  title: "Foo"
                  text: "Bar"
                  isPrivate: true
                }
              }
            ) {
              programNote { title }
            }
          }
        """,
        expected = List(
          s"User ${pi.id} is not authorized to perform this operation."
        ).asLeft
      )

  test("staff can create a private note"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = staff,
        query = s"""
          mutation {
            createProgramNote(
              input: {
                programId: "$pid"
                SET: {
                  title: "Foo"
                  text: "Bar"
                  isPrivate: true
                }
              }
            ) {
              programNote {
                title
                isPrivate
              }
            }
          }
        """,
        expected = json"""
          {
            "createProgramNote": {
              "programNote": {
                "title": "Foo",
                "isPrivate": ${true.asJson}
              }
            }
          }
        """.asRight
      )