// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.OdbError

class updateGroupsCascade extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User = TestUsers.Standard.pi(nextId, nextId)
  val validUsers: List[User] = List(pi, pi2)

  private type Elem = (Existence, Either[Group.Id, Observation.Id])

  // G holds A, B and H; H holds C and K; K holds D; S is a sibling holding E.
  private case class Tree(
    pid: Program.Id,
    g: Group.Id, h: Group.Id, k: Group.Id, s: Group.Id,
    a: Observation.Id, b: Observation.Id, c: Observation.Id, d: Observation.Id, e: Observation.Id
  ):
    val under: Set[Either[Group.Id, Observation.Id]] =
      Set(Right(a), Right(b), Left(h), Right(c), Left(k), Right(d))

  private val tree: IO[Tree] =
    for
      pid <- createProgramAs(pi)
      g   <- createGroupAs(pi, pid)
      s   <- createGroupAs(pi, pid)
      a   <- createObservationInGroupAs(pi, pid, Some(g))
      b   <- createObservationInGroupAs(pi, pid, Some(g))
      h   <- createGroupAs(pi, pid, Some(g))
      c   <- createObservationInGroupAs(pi, pid, Some(h))
      k   <- createGroupAs(pi, pid, Some(h))
      d   <- createObservationInGroupAs(pi, pid, Some(k))
      e   <- createObservationInGroupAs(pi, pid, Some(s))
    yield Tree(pid, g, h, k, s, a, b, c, d, e)

  private def allElements(gid: Group.Id, includeDeleted: Boolean): IO[List[Elem]] =
    query(
      pi,
      s"""query { group(groupId: "$gid") { allElements(includeDeleted: $includeDeleted) { existence group { id } observation { id } } } }"""
    ).map:
      _.hcursor.downFields("group", "allElements").require[List[Json]].map: e =>
        val c: io.circe.HCursor = e.hcursor
        (
          c.downField("existence").require[Existence],
          c.downFields("group", "id").as[Group.Id].toOption.toLeft(c.downFields("observation", "id").require[Observation.Id])
        )

  private def groupExistence(gid: Group.Id): IO[Existence] =
    query(pi, s"""query { group(groupId: "$gid") { existence } }""").map:
      _.hcursor.downFields("group", "existence").require[Existence]

  private def groupName(gid: Group.Id): IO[Option[String]] =
    query(pi, s"""query { group(groupId: "$gid") { name } }""").map:
      _.hcursor.downFields("group", "name").require[Option[String]]

  private def updateQuery(gid: Group.Id, set: String, cascade: Boolean): String =
    s"""mutation { updateGroups(input: { SET: { $set }, WHERE: { id: { EQ: "$gid" } }, cascade: $cascade }) { groups { id } } }"""

  private def cascadeAs(user: User, gid: Group.Id, existence: String): IO[Json] =
    query(user, updateQuery(gid, s"existence: $existence", true))

  private def assertAll(es: List[Elem], expected: Set[Either[Group.Id, Observation.Id]], existence: Existence): Unit =
    assertEquals(es.length, expected.size)
    assertEquals(es.map(_._2).toSet, expected)
    assert(es.forall(_._1 === existence), s"not all $existence: $es")

  test("cascade delete deletes everything under the group in one call"):
    for
      t   <- tree
      _   <- cascadeAs(pi, t.g, "DELETED")
      es0 <- allElements(t.g, false)
      es1 <- allElements(t.g, true)
      g   <- groupExistence(t.g)
      s   <- groupExistence(t.s)
      sel <- allElements(t.s, false)
    yield
      assertEquals(es0, Nil)
      assertAll(es1, t.under, Existence.Deleted)
      assertEquals(g, Existence.Deleted)
      assertEquals(s, Existence.Present)
      assertEquals(sel.map(_._2), List(Right(t.e)))
      assertEquals(sel.map(_._1), List(Existence.Present))

  test("delete without cascade still fails and changes nothing"):
    for
      t  <- tree
      _  <- expectOdbError(
              user = pi,
              query = updateQuery(t.g, "existence: DELETED", false),
              expected = { case OdbError.InconsistentGroupError(Some("Deleted group contains non-deleted element(s).")) => () }
            )
      g  <- groupExistence(t.g)
      es <- allElements(t.g, true)
    yield
      assertEquals(g, Existence.Present)
      assertAll(es, t.under, Existence.Present)

  test("cascade restore restores the group and everything under it"):
    for
      t  <- tree
      _  <- cascadeAs(pi, t.g, "DELETED")
      _  <- cascadeAs(pi, t.g, "PRESENT")
      g  <- groupExistence(t.g)
      es <- allElements(t.g, false)
    yield
      assertEquals(g, Existence.Present)
      assertAll(es, t.under, Existence.Present)

  test("cascade restore also restores what was deleted before the group"):
    for
      t  <- tree
      _  <- deleteObservation(pi, t.a)
      _  <- cascadeAs(pi, t.g, "DELETED")
      _  <- cascadeAs(pi, t.g, "PRESENT")
      es <- allElements(t.g, false)
    yield assertAll(es, t.under, Existence.Present)

  test("cascade delete fails, changing nothing, when an observation can no longer be deleted"):
    for
      t  <- tree
      _  <- declareOngoingDirectly(t.d)
      _  <- expectOdbError(
              user = pi,
              query = updateQuery(t.g, "existence: DELETED", true),
              expected = {
                case OdbError.InvalidObservation(oid, _) if oid === t.d                    => ()
                case OdbError.InvalidArgument(Some(msg)) if msg.contains(t.d.toString) => ()
              }
            )
      g  <- groupExistence(t.g)
      es <- allElements(t.g, true)
    yield
      assertEquals(g, Existence.Present)
      assertAll(es, t.under, Existence.Present)

  test("cascade with a SET that does not change existence is a plain update"):
    for
      t  <- tree
      _  <- query(pi, updateQuery(t.g, """name: "renamed"""", true))
      n  <- groupName(t.g)
      es <- allElements(t.g, true)
    yield
      assertEquals(n, Some("renamed"))
      assertAll(es, t.under, Existence.Present)

  test("another PI's cascade delete changes nothing"):
    for
      t  <- tree
      r  <- cascadeAs(pi2, t.g, "DELETED")
      g  <- groupExistence(t.g)
      es <- allElements(t.g, true)
    yield
      assertEquals(r.hcursor.downFields("updateGroups", "groups").require[List[Json]], Nil)
      assertEquals(g, Existence.Present)
      assertAll(es, t.under, Existence.Present)
}
