// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.ACursorOps
import lucuma.odb.graphql.TestUsers
import lucuma.odb.util.Codecs.group_id
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.user_id
import skunk.syntax.all.*

class groups extends GroupTestSupport:

  val guest: User = TestUsers.guest(5)

  override val validUsers: List[User] =
    List(pi, pi2, serviceUser, staff, guest)

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

  test("staff see every program's groups, and a guest only their own"):
    for
      p  <- createProgram
      g  <- createGroupAs(pi, p)
      gp <- createProgramAs(guest)
      gg <- createGroupAs(guest, gp)
      s  <- groups(staff, s"WHERE: { ${inProgram(p)} }")
      sg <- groups(staff, s"WHERE: { ${inProgram(gp)} }")
      r  <- groups(guest, s"WHERE: { ${inProgram(p)} }")
      rg <- groups(guest, s"WHERE: { ${inProgram(gp)} }")
    yield
      assertEquals(s, (List(g), false))
      assertEquals(sg, (List(gg), false))
      assertEquals(r, (Nil, false))
      assertEquals(rg, (List(gg), false))

  test("a deleted group has no standing, so it matches no standing filter"):
    for
      p   <- createProgram
      g1  <- createGroupAs(pi, p)
      g2  <- createGroupAs(pi, p)
      _   <- deleteGroup(g2)
      all <- groups(pi, s"WHERE: { ${inProgram(p)} }, includeDeleted: true")
      v   <- groups(pi, s"WHERE: { ${inProgram(p)}, standing: { status: { EQ: VIABLE } } }, includeDeleted: true")
      n   <- groups(pi, s"WHERE: { ${inProgram(p)}, standing: { status: { NEQ: COMPLETE } } }, includeDeleted: true")
    yield
      assertEquals(all, (List(g1, g2), false))
      assertEquals(v, (List(g1), false))
      assertEquals(n, (List(g1), false))

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

  test("groups of deleted programs are left out unless asked for"):
    for
      p  <- createProgram
      g  <- createGroupAs(pi, p)
      _  <- query(pi, s"""mutation { updatePrograms(input: { SET: { existence: DELETED }, WHERE: { id: { EQ: "$p" } } }) { programs { id } } }""")
      r  <- groups(pi, s"WHERE: { ${inProgram(p)} }")
      rd <- groups(pi, s"WHERE: { ${inProgram(p)} }, includeDeleted: true")
    yield
      assertEquals(r, (Nil, false))
      assertEquals(rd, (List(g), false))

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

  // A group with a revoked ORDER waiver at o1 and an active MINIMUM_INTERVAL
  // waiver after o2, another with none, and a third with only a revoked
  // per-night maximum waiver, written directly since the violations needn't
  // have happened.
  case class Waived(p: Program.Id, g: Group.Id, g2: Group.Id, g3: Group.Id, o1: Observation.Id, o2: Observation.Id)

  val waived: IO[Waived] =
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
    yield Waived(p, g, g2, g3, o1, o2)

  def byWaivers(pid: Program.Id, where: String): IO[List[Group.Id]] =
    byGroup(pid, s"waivers: $where")

  def byGroup(pid: Program.Id, where: String): IO[List[Group.Id]] =
    groups(pi, s"WHERE: { ${inProgram(pid)}, $where }").map(_._1)

  def waiverIds(gid: Group.Id): IO[List[Json]] =
    query(pi, s"""query { group(groupId: "$gid") { waivers(includeRevoked: true) { id } } }""")
      .map(_.hcursor.downFields("group", "waivers").require[List[Json]].map(_.hcursor.downField("id").focus.get))

  test("groups may be found by their waivers' fields"):
    for
      w   <- waived
      ids <- waiverIds(w.g3)
      rs  <- List(
               s"{ id: { EQ: ${ids.head} } }",
               "{ violation: { code: { EQ: ORDER } } }",
               "{ violation: { code: { EQ: SAME_NIGHT } } }",
               s"""{ violation: { element: { observationId: { EQ: "${w.o2}" } } } }""",
               s"""{ violation: { element: { groupId: { EQ: "${w.g}" } } } }""",
               "{ violation: { night: { site: { EQ: GN } } } }",
               "{ violation: { night: { site: { EQ: GN }, date: { EQ: \"2026-10-16\" } } } }",
               "{ violation: { night: { date: { GT: \"2026-10-16\" } } } }",
               s"""{ granted: { by: { EQ: "${pi.id}" }, at: { GT: "1970-01-01 00:00:00" } } }""",
               "{ granted: { at: { LT: \"1970-01-01 00:00:00\" } } }",
               "{ revoked: { IS_NULL: true } }",
               "{ revoked: { IS_NULL: false } }",
               s"""{ revoked: { by: { EQ: "${pi.id}" } } }""",
               "{ revoked: { at: { GT: \"1970-01-01 00:00:00\" } } }",
               "{ revoked: { at: { LT: \"1970-01-01 00:00:00\" } } }"
             ).traverse(byWaivers(w.p, _))
    yield assertEquals(
      rs,
      List(
        List(w.g3),
        List(w.g),
        Nil,
        List(w.g),
        Nil,
        List(w.g3),
        List(w.g3),
        Nil,
        List(w.g, w.g3),
        Nil,
        List(w.g),
        List(w.g, w.g3),
        List(w.g, w.g3),
        List(w.g, w.g3),
        Nil
      )
    )

  test("the conditions of a waiver filter all apply to the same waiver"):
    for
      w           <- waived
      activeOrder <- byWaivers(w.p, "{ violation: { code: { EQ: ORDER } }, revoked: { IS_NULL: true } }")
      activeMin   <- byWaivers(w.p, "{ violation: { code: { EQ: MINIMUM_INTERVAL } }, revoked: { IS_NULL: true } }")
      andOrder    <- byWaivers(w.p, "{ AND: [{ violation: { code: { EQ: ORDER } } }, { revoked: { IS_NULL: true } }] }")
    yield
      assertEquals(activeOrder, Nil)
      assertEquals(activeMin, List(w.g))
      assertEquals(andOrder, Nil)

  // A group without waivers has nulls in their place, which a test for null
  // would match.
  test("a group without waivers matches no waiver condition, negated or not"):
    for
      w   <- waived
      rs  <- List(
               "waivers: { revoked: { IS_NULL: true } }",
               "waivers: { revoked: { IS_NULL: false } }",
               "waivers: { NOT: { revoked: { IS_NULL: true } } }",
               "NOT: { waivers: { revoked: { IS_NULL: false } } }",
               "waivers: { NOT: { violation: { code: { EQ: ORDER } } } }"
             ).traverse(byGroup(w.p, _))
    yield assert(!rs.flatten.contains(w.g2), rs.toString)

  // As documented: a negated waiver filter matches a group with some waiver
  // that doesn't match, whether negated inside or out, and so never a group
  // without waivers.
  test("a negated waiver filter matches some waiver that doesn't match"):
    for
      w       <- waived
      inside  <- byWaivers(w.p, "{ NOT: { revoked: { IS_NULL: true } } }")
      outside <- byGroup(w.p, "NOT: { waivers: { revoked: { IS_NULL: true } } }")
    yield
      assertEquals(inside, List(w.g, w.g3))
      assertEquals(outside, inside)

  test("groups may be found by whether they have waivers, active or not"):
    for
      w        <- waived
      any      <- byGroup(w.p, "hasWaiver: { EQ: true }")
      none     <- byGroup(w.p, "hasWaiver: { EQ: false }")
      active   <- byGroup(w.p, "hasActiveWaiver: { EQ: true }")
      noActive <- byGroup(w.p, "hasActiveWaiver: { EQ: false }")
    yield
      assertEquals(any, List(w.g, w.g3))
      assertEquals(none, List(w.g2))
      assertEquals(active, List(w.g))
      assertEquals(noActive, List(w.g2, w.g3))

  def setGroup(gid: Group.Id, set: String): IO[Unit] =
    query(pi, s"""mutation { updateGroups(input: { SET: { $set }, WHERE: { id: { EQ: "$gid" } } }) { groups { id } } }""").void

  test("groups may be found by their constraints"):
    for
      p        <- createProgram
      plain    <- createGroupAs(pi, p)
      ordered  <- createGroupAs(pi, p, minRequired = Some(NonNegShort.unsafeFrom(2)), minimumInterval = Some(1.hourTimeSpan), maximumInterval = Some(24.hourTimeSpan))
      _        <- setGroup(ordered, "ordered: true")
      night    <- createGroupAs(pi, p)
      _        <- setGroup(night, "sameNight: true")
      perNight <- createGroupAs(pi, p)
      _        <- setGroup(perNight, "minimumObservationsPerNight: 1, maximumObservationsPerNight: 3")
      all       = List(plain, ordered, night, perNight)
      rs       <- List(
                    "ordered: { EQ: true }",
                    "sameNight: { EQ: true }",
                    "system: { EQ: false }",
                    "system: { EQ: true }",
                    "minimumRequired: { IS_NULL: true }",
                    "minimumRequired: { GTE: 2 }",
                    "minimumInterval: { EQ: { hours: 1 } }",
                    "maximumInterval: { LT: { hours: 48 } }",
                    "maximumInterval: { LT: { hours: 12 } }",
                    "maximumInterval: { IS_NULL: false }",
                    "minimumObservationsPerNight: { IS_NULL: false }",
                    "maximumObservationsPerNight: { LTE: 2 }",
                    "maximumObservationsPerNight: { LTE: 3 }",
                    "minimumInterval: { IS_NULL: true }",
                    "maximumInterval: { IS_NULL: true }",
                    "minimumObservationsPerNight: { IS_NULL: true }",
                    "maximumObservationsPerNight: { IS_NULL: true }"
                  ).traverse(byGroup(p, _))
    yield assertEquals(
      rs,
      List(
        List(ordered),
        List(night),
        all,
        Nil,
        List(plain, night, perNight),
        List(ordered),
        List(ordered),
        List(ordered),
        Nil,
        List(ordered),
        List(perNight),
        Nil,
        List(perNight),
        List(plain, night, perNight),
        List(plain, night, perNight),
        List(plain, ordered, night),
        List(plain, ordered, night)
      )
    )

