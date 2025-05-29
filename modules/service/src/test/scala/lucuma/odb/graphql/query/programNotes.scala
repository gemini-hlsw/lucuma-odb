// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ProgramNote

class programNotes extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 30)
  val pi2   = TestUsers.Standard.pi(2, 32)
  val staff = TestUsers.Standard.staff(5, 34)

  val validUsers = List(pi, pi2, staff)

  test("no WHERE"):
    (createProgramAs(pi).flatMap: pid =>
      createProgramNoteAs(pi, pid, "Foo", "Bar".some)
    )
      .replicateA(5)
      .flatMap: nids =>
        expect(
          user  = pi,
          query = s"""
            query {
              programNotes() {
                hasMore
                matches { id }
              }
            }
          """,
          expected = Json.obj(
            "programNotes" -> Json.obj(
              "hasMore" -> Json.False,
              "matches" -> Json.fromValues(
                nids.map: nid =>
                  Json.obj("id" -> nid.asJson)
              )
            )
          ).asRight
        )

  test("LIMIT"):
    (createProgramAs(pi2).flatMap: pid =>
      createProgramNoteAs(pi2, pid, "Foo", "Bar".some)
    )
      .replicateA(5)
      .flatMap: nids =>
        expect(
          user  = pi2,
          query = s"""
            query {
              programNotes(LIMIT: 4) {
                hasMore
                matches { id }
              }
            }
          """,
          expected = Json.obj(
            "programNotes" -> Json.obj(
              "hasMore" -> Json.True,
              "matches" -> Json.fromValues(
                nids.take(4).map: nid =>
                  Json.obj("id" -> nid.asJson)
              )
            )
          ).asRight
        )

  test("WHERE title"):
    (
      (0 until 3).toList.traverse: n =>
        createProgramAs(pi).flatMap: pid =>
          createProgramNoteAs(pi, pid, s"Foo$n", "Bar".some)
    )
      .flatMap: nids =>
        expect(
          user  = pi,
          query = s"""
            query {
              programNotes(WHERE: { title: { EQ: "Foo1" } }) {
                hasMore
                matches { id }
              }
            }
          """,
          expected = Json.obj(
            "programNotes" -> Json.obj(
              "hasMore" -> Json.False,
              "matches" -> Json.arr(
                Json.obj("id" -> nids(1).asJson)
              )
            )
          ).asRight
        )

  test("filter private"):
    (
      (0 until 4).toList.traverse: n =>
        createProgramAs(pi).flatMap: pid =>
          val isPrivate = n % 2 === 0
          createProgramNoteAs(staff, pid, s"Test Private", "Bar".some, isPrivate = isPrivate.some)
    )
      .flatMap: nids =>
        expect(
          user  = pi,
          query = s"""
            query {
              programNotes(WHERE: { title: { EQ: "Test Private" } }) {
                hasMore
                matches { id }
              }
            }
          """,
          expected = Json.obj(
            "programNotes" -> Json.obj(
              "hasMore" -> Json.False,
              "matches" -> Json.arr(
                Json.obj("id" -> nids(1).asJson),
                Json.obj("id" -> nids(3).asJson),
              )
            )
          ).asRight
        )

  test("WHERE isPrivate"):
    (
      (0 until 4).toList.traverse: n =>
        createProgramAs(pi).flatMap: pid =>
          val isPrivate = n % 2 === 0
          createProgramNoteAs(staff, pid, s"WHERE isPrivate Test", "Bar".some, isPrivate = isPrivate.some)
    )
      .flatMap: nids =>
        expect(
          user  = staff,
          query = s"""
            query {
              programNotes(WHERE: {
                title: { EQ: "WHERE isPrivate Test" },
                isPrivate: { EQ: true }
              }) {
                hasMore
                matches { id }
              }
            }
          """,
          expected = Json.obj(
            "programNotes" -> Json.obj(
              "hasMore" -> Json.False,
              "matches" -> Json.arr(
                Json.obj("id" -> nids(0).asJson),
                Json.obj("id" -> nids(2).asJson)
              )
            )
          ).asRight
        )
  test("WHERE program"):
    (
      (0 until 4).toList.traverse: n =>
        createProgramAs(pi).flatMap: pid =>
          createProgramNoteAs(staff, pid, s"WHERE Program $n", "Bar".some).as(pid)
    )
      .flatMap: pids =>
        expect(
          user  = staff,
          query = s"""
            query {
              programNotes(WHERE: {
                program: { id: { EQ: "${pids(1)}" } }
              }) {
                hasMore
                matches { title }
              }
            }
          """,
          expected = Json.obj(
            "programNotes" -> Json.obj(
              "hasMore" -> Json.False,
              "matches" -> Json.arr(
                Json.obj("title" -> "WHERE Program 1".asJson)
              )
            )
          ).asRight
        )
