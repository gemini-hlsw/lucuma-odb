// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.literal.*
import lucuma.core.model.ProgramNote

class programNote extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 101)
  val pi2   = TestUsers.Standard.pi(2, 102)
  val staff = TestUsers.Standard.staff(3, 103)

  val validUsers = List(pi, pi2, staff)

  def programNoteQuery(nid: ProgramNote.Id): String = s"""
    query {
      programNote(programNoteId: "$nid") {
        title
        text
      }
    }
  """

  test("can read your own notes"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some).flatMap: nid =>
        expect(
          user     = pi,
          query    = programNoteQuery(nid),
          expected = json"""
            {
              "programNote": {
                "title": "Foo",
                "text": "Bar"
              }
            }
          """.asRight
        )

  test("cannot read another PI's notes"):
    createProgramAs(pi).flatMap: pid =>
      createProgramAs(pi2) *>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some).flatMap: nid =>
        expect(
          user     = pi2,
          query    = programNoteQuery(nid),
          expected = json"""
            {
              "programNote": null
            }
          """.asRight
        )

  test("cannot read private notes as a PI"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(staff, pid, "Foo", "Bar".some, isPrivate = true.some).flatMap: nid =>
        expect(
          user     = pi,
          query    = programNoteQuery(nid),
          expected = json"""
            {
              "programNote": null
            }
          """.asRight
        )

  test("staff can read private notes"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(staff, pid, "Foo", "Bar".some, isPrivate = true.some).flatMap: nid =>
        expect(
          user     = staff,
          query    = programNoteQuery(nid),
          expected = json"""
            {
              "programNote": {
                "title": "Foo",
                "text": "Bar"
              }
            }
          """.asRight
        )
