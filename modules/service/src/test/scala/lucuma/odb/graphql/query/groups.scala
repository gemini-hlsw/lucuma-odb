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
import lucuma.odb.util.Codecs.group_id
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.user_id
import skunk.syntax.all.*

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

  // A group with a revoked ORDER waiver and an active MINIMUM_INTERVAL waiver,
  // another with none, and a third with only a revoked per-night maximum
  // waiver, written directly since the violations needn't have happened.
  val waived: IO[(Program.Id, Group.Id, Group.Id, Group.Id)] =
    for
      p  <- createProgram
      o1 <- createObservationAs(pi, p)
      o2 <- createObservationAs(pi, p)
      g  <- group(p, List(o1, o2))
      g2 <- createGroupAs(pi, p)
      g3 <- createGroupAs(pi, p)
      _  <- withSession(_.execute(
              sql"""
                INSERT INTO t_group_constraint_waiver (c_group_id, c_code, c_element_observation_id, c_night_site, c_night_date, c_granted_by, c_revoked_by, c_revoked_at)
                VALUES ($group_id, 'order',                          $observation_id, NULL, NULL,         $user_id, $user_id, now()),
                       ($group_id, 'minimum_interval',               $observation_id, NULL, NULL,         $user_id, NULL,     NULL),
                       ($group_id, 'observations_per_night_maximum', NULL,            'gn', '2026-10-16', $user_id, $user_id, now())
              """.command
            )((g, o1, pi.id, pi.id, g, o2, pi.id, g3, pi.id, pi.id)))
    yield (p, g, g2, g3)

  def byWaivers(pid: Program.Id, where: String): IO[List[Group.Id]] =
    byGroup(pid, s"waivers: $where")

  def byGroup(pid: Program.Id, where: String): IO[List[Group.Id]] =
    groups(pi, s"WHERE: { ${inProgram(pid)}, $where }").map(_._1)

  test("groups may be found by their waivers"):
    for
      (p, g, _, g3) <- waived
      order         <- byWaivers(p, "{ code: { EQ: ORDER } }")
      revoked       <- byWaivers(p, "{ active: { EQ: false } }")
      by            <- byWaivers(p, s"""{ grantedBy: { EQ: "${pi.id}" } }""")
      sameNight     <- byWaivers(p, "{ code: { EQ: SAME_NIGHT } }")
    yield
      assertEquals(order, List(g))
      assertEquals(revoked, List(g, g3))
      assertEquals(by, List(g, g3))
      assertEquals(sameNight, Nil)

  test("the conditions of a waiver filter all apply to the same waiver"):
    for
      (p, g, _, _) <- waived
      activeOrder  <- byWaivers(p, "{ code: { EQ: ORDER }, active: { EQ: true } }")
      activeMin    <- byWaivers(p, "{ code: { EQ: MINIMUM_INTERVAL }, active: { EQ: true } }")
      andOrder     <- byWaivers(p, "{ AND: [{ code: { EQ: ORDER } }, { active: { EQ: true } }] }")
    yield
      assertEquals(activeOrder, Nil)
      assertEquals(activeMin, List(g))
      assertEquals(andOrder, Nil)

  // As documented: a negated waiver filter matches a group with some waiver
  // that doesn't match, whether negated inside or out, and so never a group
  // without waivers.
  test("a negated waiver filter matches some waiver that doesn't match"):
    for
      (p, g, _, g3) <- waived
      inside        <- byWaivers(p, "{ NOT: { active: { EQ: true } } }")
      outside       <- byGroup(p, "NOT: { waivers: { active: { EQ: true } } }")
    yield
      assertEquals(inside, List(g, g3))
      assertEquals(outside, inside)

  test("groups may be found by whether they have waivers, active or not"):
    for
      (p, g, g2, g3) <- waived
      any            <- byGroup(p, "hasWaiver: { EQ: true }")
      none           <- byGroup(p, "hasWaiver: { EQ: false }")
      active         <- byGroup(p, "hasActiveWaiver: { EQ: true }")
      noActive       <- byGroup(p, "hasActiveWaiver: { EQ: false }")
    yield
      assertEquals(any, List(g, g3))
      assertEquals(none, List(g2))
      assertEquals(active, List(g))
      assertEquals(noActive, List(g2, g3))
