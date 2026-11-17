// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.ACursorOps

class groups extends GroupTestSupport:

  // The ids of the matching groups, and whether there are more.
  def groups(user: User, args: String): IO[(List[Group.Id], Boolean)] =
    query(user, s"""query { groups($args) { matches { id } hasMore } }""").map: js =>
      val c = js.hcursor.downField("groups")
      (
        c.downField("matches").require[List[Json]].map(_.hcursor.downField("id").require[Group.Id]),
        c.downField("hasMore").require[Boolean]
      )

  def inProgram(pid: Program.Id): String =
    s"""program: { id: { EQ: "$pid" } }"""

  def deleteGroup(gid: Group.Id): IO[Unit] =
    query(pi, s"""mutation { updateGroups(input: { SET: { existence: DELETED }, WHERE: { id: { EQ: "$gid" } } }) { groups { id } } }""").void

  test("a user sees the groups of the programs they may see, ordered by id"):
    for
      p  <- createProgram
      g1 <- createGroupAs(pi, p)
      g2 <- createGroupAs(pi, p)
      r  <- groups(pi, s"WHERE: { ${inProgram(p)} }")
      r2 <- groups(pi2, s"WHERE: { ${inProgram(p)} }")
    yield
      assertEquals(r, (List(g1, g2), false))
      assertEquals(r2, (Nil, false))

  test("deleted groups are left out unless asked for"):
    for
      p  <- createProgram
      g1 <- createGroupAs(pi, p)
      g2 <- createGroupAs(pi, p)
      _  <- deleteGroup(g2)
      r  <- groups(pi, s"WHERE: { ${inProgram(p)} }")
      rd <- groups(pi, s"WHERE: { ${inProgram(p)} }, includeDeleted: true")
    yield
      assertEquals(r, (List(g1), false))
      assertEquals(rd, (List(g1, g2), false))

  test("groups are paged by LIMIT, OFFSET_AT and OFFSET_AFTER"):
    for
      p  <- createProgram
      g1 <- createGroupAs(pi, p)
      g2 <- createGroupAs(pi, p)
      g3 <- createGroupAs(pi, p)
      r1 <- groups(pi, s"WHERE: { ${inProgram(p)} }, LIMIT: 2")
      r2 <- groups(pi, s"""WHERE: { ${inProgram(p)} }, LIMIT: 2, OFFSET_AFTER: "$g2"""")
      r3 <- groups(pi, s"""WHERE: { ${inProgram(p)} }, OFFSET_AT: "$g2"""")
    yield
      assertEquals(r1, (List(g1, g2), true))
      assertEquals(r2, (List(g3), false))
      assertEquals(r3, (List(g2, g3), false))

  test("groups may be found by their standing"):
    for
      p   <- createProgram
      o1  <- newObservation(p)
      o2  <- newObservation(p)
      bad <- group(p, List(o1, o2))
      _   <- query(pi, s"""mutation { updateGroups(input: { SET: { ordered: true }, WHERE: { id: { EQ: "$bad" } } }) { groups { id } } }""")
      ok  <- createGroupAs(pi, p)
      _   <- completeObservation(o2)
      _   <- refreshGroupStandingsAs(serviceUser)
      u   <- groups(pi, s"WHERE: { ${inProgram(p)}, standing: { status: { EQ: UNSCHEDULABLE } } }")
      v   <- groups(pi, s"WHERE: { ${inProgram(p)}, standing: { status: { EQ: VIABLE }, calculationState: { EQ: READY } } }")
    yield
      assertEquals(u, (List(bad), false))
      assertEquals(v, (List(ok), false))

  test("the group filters apply to updateGroups too"):
    for
      p  <- createProgram
      p2 <- createProgram
      g1 <- createGroupAs(pi, p)
      _  <- createGroupAs(pi, p2)
      js <- query(pi, s"""mutation { updateGroups(input: { SET: { name: "renamed" }, WHERE: { ${inProgram(p)} } }) { groups { id } } }""")
    yield assertEquals(
      js.hcursor.downFields("updateGroups", "groups").focus.get,
      json"""[{ "id": $g1 }]"""
    )
