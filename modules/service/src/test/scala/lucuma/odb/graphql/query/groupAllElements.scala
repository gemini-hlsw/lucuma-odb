// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User

class groupAllElements extends OdbSuite with DatabaseOperations {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val validUsers: List[User] = List(pi)

  // (parent group, parent index, element)
  type Elem = (Option[Group.Id], Int, Either[Group.Id, Observation.Id])

  private def allElementsQuery(gid: Group.Id, includeDeleted: Boolean): String =
    s"""query { group(groupId: "$gid") { allElements(includeDeleted: $includeDeleted) { parentGroupId parentIndex group { id } observation { id } } } }"""

  private def parseElems(json: Json): List[Elem] =
    json.hcursor
      .downFields("group", "allElements")
      .require[List[Json]]
      .map: e =>
        val c: io.circe.HCursor = e.hcursor
        val elem: Either[Group.Id, Observation.Id] =
          c.downFields("group", "id").as[Group.Id].toOption.toLeft(c.downFields("observation", "id").require[Observation.Id])
        (c.downField("parentGroupId").require[Option[Group.Id]], c.downField("parentIndex").require[Int], elem)

  private def allElements(gid: Group.Id, includeDeleted: Boolean = false): IO[List[Elem]] =
    query(pi, allElementsQuery(gid, includeDeleted)).map(parseElems)

  private def obs(pid: Program.Id, gid: Group.Id): IO[Observation.Id] =
    createObservationInGroupAs(pi, pid, Some(gid))

  // G holds A, B and H; H holds C and K; K holds D; S is a sibling holding E.
  private case class Tree(
    pid: Program.Id,
    g: Group.Id, h: Group.Id, k: Group.Id, s: Group.Id,
    a: Observation.Id, b: Observation.Id, c: Observation.Id, d: Observation.Id, e: Observation.Id
  )

  private val tree: IO[Tree] =
    for
      pid <- createProgramAs(pi)
      g   <- createGroupAs(pi, pid)
      s   <- createGroupAs(pi, pid)
      a   <- obs(pid, g)
      b   <- obs(pid, g)
      h   <- createGroupAs(pi, pid, Some(g))
      c   <- obs(pid, h)
      k   <- createGroupAs(pi, pid, Some(h))
      d   <- obs(pid, k)
      e   <- obs(pid, s)
    yield Tree(pid, g, h, k, s, a, b, c, d, e)

  test("lists every element under a group at any depth, in parent and index order"):
    tree.flatMap: t =>
      allElements(t.g).map: es =>
        assertEquals(
          es.map((p, _, e) => (p, e)).toSet,
          Set[(Option[Group.Id], Either[Group.Id, Observation.Id])](
            (Some(t.g), Right(t.a)),
            (Some(t.g), Right(t.b)),
            (Some(t.g), Left(t.h)),
            (Some(t.h), Right(t.c)),
            (Some(t.h), Left(t.k)),
            (Some(t.k), Right(t.d))
          )
        )
        assertEquals(es.length, 6)
        val keys: List[(Group.Id, Int)] = es.map((p, i, _) => (p.get, i))
        assertEquals(keys, keys.sortBy((p, i) => (p, i)))

  test("an empty group has no elements"):
    for
      pid <- createProgramAs(pi)
      g   <- createGroupAs(pi, pid)
      es  <- allElements(g)
    yield assertEquals(es, Nil)

  test("deleted observations are listed only when requested"):
    for
      pid <- createProgramAs(pi)
      g   <- createGroupAs(pi, pid)
      a   <- obs(pid, g)
      x   <- obs(pid, g)
      _   <- deleteObservation(pi, x)
      es0 <- allElements(g)
      es1 <- allElements(g, includeDeleted = true)
    yield
      assertEquals(es0.map(_._3), List(Right(a)))
      assertEquals(es1.map(_._3).toSet, Set[Either[Group.Id, Observation.Id]](Right(a), Right(x)))

  test("observations may select more than one list"):
    tree.flatMap: t =>
      query(
        pi,
        s"""query { group(groupId: "${t.g}") { allElements { observation { id targetEnvironment { asterism { id } } attachments { id } } } } }"""
      ).map: json =>
        val ids: List[Observation.Id] =
          json.hcursor.downFields("group", "allElements").require[List[Json]]
            .flatMap(_.hcursor.downFields("observation", "id").as[Observation.Id].toOption)
        assertEquals(ids.toSet, Set(t.a, t.b, t.c, t.d))

  private def cloneAllElements(gid: Group.Id): String =
    s"""mutation { cloneGroup(input: { groupId: "$gid" }) { newGroup { id allElements { parentGroupId group { id } observation { id } } } } }"""

  test("allElements works in the cloneGroup reply"):
    tree.flatMap: t =>
      for
        orig  <- allElements(t.g)
        json  <- query(pi, cloneAllElements(t.g))
        cid   <- json.hcursor.downFields("cloneGroup", "newGroup", "id").require[Group.Id].pure[IO]
        elems <- json.hcursor.downFields("cloneGroup", "newGroup", "allElements").require[List[Json]].pure[IO]
      yield
        val parsed: List[(Option[Group.Id], Either[Group.Id, Observation.Id])] = elems.map: e =>
          val c: io.circe.HCursor = e.hcursor
          (c.downField("parentGroupId").require[Option[Group.Id]],
           c.downFields("group", "id").as[Group.Id].toOption.toLeft(c.downFields("observation", "id").require[Observation.Id]))
        val origIds: Set[Either[Group.Id, Observation.Id]] = orig.map(_._3).toSet + Left(t.g)
        assertEquals(parsed.length, orig.length)
        assertEquals(parsed.count(_._2.isLeft), orig.count(_._3.isLeft))
        assertEquals(parsed.count(_._2.isRight), orig.count(_._3.isRight))
        assert(parsed.forall(p => !origIds.contains(p._2)), "clone reuses an original id")
        assert(!origIds.contains(Left(cid)))
        // nesting: map each clone parent to its depth below the clone
        def depth(p: Option[Group.Id]): Int =
          if p === Some(cid) then 0
          else parsed.find(_._2 === p.get.asLeft[Observation.Id]).fold(-1)(x => 1 + depth(x._1))
        val origDepth: Map[Group.Id, Int] = Map(t.g -> 0, t.h -> 1, t.k -> 2)
        def odepth(p: Option[Group.Id]): Int = origDepth(p.get)
        assertEquals(parsed.map(p => (depth(p._1), p._2.isLeft)).sorted, orig.map(e => (odepth(e._1), e._3.isLeft)).sorted)
}
