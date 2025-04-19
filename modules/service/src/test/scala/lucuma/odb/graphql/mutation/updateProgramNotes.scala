// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.User

class updateProgramNotes extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 101)
  val pi2   = TestUsers.Standard.pi(2, 102)
  val staff = TestUsers.Standard.staff(3, 103)

  val validUsers = List(pi, pi2, staff)

  test("simple edit"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some) *>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            updateProgramNotes(input: {
              SET: {
                title: "Baz"
              }
              WHERE: {
                title: { EQ: "Foo" }
              }
            }) {
              hasMore
              programNotes {
                title
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProgramNotes": {
              "hasMore": false,
              "programNotes": [
                {
                  "title": "Baz"
                }
              ]
            }
          }
        """.asRight
      )

  test("pi cannot make a private note visible"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(staff, pid, "Private Foo", "Bar".some, isPrivate = true.some) *>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            updateProgramNotes(input: {
              SET: {
                isPrivate: false
              }
              WHERE: {
                title: { EQ: "Private Foo" }
              }
            }) {
              hasMore
              programNotes {
                title
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProgramNotes": {
              "hasMore": false,
              "programNotes": []
            }
          }
        """.asRight
      )

  test("staff can make a private note visible"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(staff, pid, "Private Foo 2", "Bar".some, isPrivate = true.some) *>
      expect(
        user     = staff,
        query    = s"""
          mutation {
            updateProgramNotes(input: {
              SET: {
                isPrivate: false
              }
              WHERE: {
                title: { EQ: "Private Foo 2" }
              }
            }) {
              hasMore
              programNotes {
                title
                isPrivate
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProgramNotes": {
              "hasMore": false,
              "programNotes": [
                {
                  "title": "Private Foo 2",
                  "isPrivate": false
                }
              ]
            }
          }
        """.asRight
      )

  test("pis cannot see another pi's notes"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Quz", "Bar".some) *>
      createProgramAs(pi2).flatMap: pid2 =>
        createProgramNoteAs(pi2, pid2, "Quz", "Bar".some).flatMap: nid =>
          expect(
            user     = pi2,
            query    = s"""
              mutation {
                updateProgramNotes(input: {
                  SET: {
                    title: "Quo"
                  }
                  WHERE: {
                    title: { EQ: "Quz" }
                  }
                }) {
                  hasMore
                  programNotes {
                    id
                  }
                }
              }
            """,
            expected = json"""
              {
                "updateProgramNotes": {
                  "hasMore": false,
                  "programNotes": [
                    {
                      "id": ${nid.asJson}
                    }
                  ]
                }
              }
            """.asRight
          )

  test("can delete text"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some).flatMap: nid =>
        expect(
          user     = pi,
          query    = s"""
            mutation {
              updateProgramNotes(input: {
                SET: {
                  text: null
                }
                WHERE: {
                  id: { EQ: "$nid" }
                }
              }) {
                hasMore
                programNotes {
                  text
                }
              }
            }
          """,
          expected = json"""
            {
              "updateProgramNotes": {
                "hasMore": false,
                "programNotes": [
                  {
                    "text": null
                  }
                ]
              }
            }
          """.asRight
        )

  test("can delete notes"):
    createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some).flatMap: nid =>
        expect(
          user     = pi,
          query    = s"""
            mutation {
              updateProgramNotes(input: {
                SET: {
                  existence: DELETED
                }
                WHERE: {
                  id: { EQ: "$nid" }
                }
              }) {
                hasMore
                programNotes {
                  existence
                }
              }
            }
          """,
          expected = json"""
            {
              "updateProgramNotes": {
                "hasMore": false,
                "programNotes": [
                  {
                    "existence": "DELETED"
                  }
                ]
              }
            }
          """.asRight
        ) *>
        expect(
          user  = pi,
          query = s"""
            query {
              program(programId: "$pid") {
                notes { title }
              }
            }
          """,
          expected = json"""
            {
              "program": {
                "notes": []
              }
            }
          """.asRight
        )

