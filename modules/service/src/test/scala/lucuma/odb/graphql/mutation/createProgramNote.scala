// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence

class createProgramNote extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)

  val validUsers = List(pi, staff)

  def listNotesAs(user: User, pid: Program.Id, includeDeleted: Option[Boolean] = none): IO[List[String]] =
    query(
      user  = user,
      query = s"""
        query {
          program(programId: "$pid") {
            notes${includeDeleted.fold("")(b => s"(includeDeleted: $b)")} { title }
          }
        }
      """
    ).map: json =>
      json.hcursor.downFields("program", "notes").values.toList.flatMap(_.toList).map: json =>
        json.hcursor.downField("title").require[String]


  test("one note"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some) *>
      assertIO(listNotesAs(pi, pid), List("Foo"))

  test("two notes"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some) *>
      createProgramNoteAs(pi, pid, "Baz", "Buz".some) *>
      assertIO(listNotesAs(pi, pid), List("Foo", "Baz"))

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

  test("staff can create and see a private note"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(staff, pid, "Foo", "Bar".some, isPrivate = true.some) *>
      assertIO(listNotesAs(staff, pid), List("Foo"))

  test("pi cannot see a private note"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(staff, pid, "Foo", "Bar".some, isPrivate = true.some) *>
      assertIO(listNotesAs(pi, pid), Nil)

  test("deleted notes are not listed by default"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some, existence = Existence.Deleted.some) *>
      assertIO(listNotesAs(pi, pid), Nil)

  test("deleted notes are listed when requested"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some, existence = Existence.Deleted.some) *>
      assertIO(listNotesAs(pi, pid, includeDeleted = true.some), List("Foo"))
