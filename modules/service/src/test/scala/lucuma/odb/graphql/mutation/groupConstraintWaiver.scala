// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mutation

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.ACursorOps
import lucuma.odb.graphql.query.GroupTestSupport
import lucuma.odb.util.Codecs.group_id
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.user_id
import skunk.codec.numeric.int8
import skunk.syntax.all.*

import java.time.LocalDate
import scala.concurrent.duration.*

class groupConstraintWaiver extends GroupTestSupport:

  val WaiverFields: String =
    """
      id
      groupId
      violation {
        code
        element { groupId observationId }
        night { site date }
        subgroupId
      }
      granted { by at reason }
      revoked { by at reason }
    """

  def waive(user: User, gid: Group.Id, violation: String, reason: Option[String] = None): IO[Json] =
    query(user, waiveMutation(gid, violation, reason))
      .map(_.hcursor.downFields("waiveGroupConstraint", "waiver").focus.get)

  def waiveMutation(gid: Group.Id, violation: String, reason: Option[String] = None): String =
    val r = reason.fold("")(r => s""", reason: "$r"""")
    s"""
      mutation {
        waiveGroupConstraint(input: { groupId: "$gid", violation: $violation$r }) {
          waiver { $WaiverFields }
        }
      }
    """

  def orderAt(oid: Observation.Id): String =
    s"""{ code: ORDER, element: { observationId: "$oid" } }"""

  def revoke(user: User, wid: Json, reason: Option[String] = None): IO[Json] =
    query(user, revokeMutation(wid, reason))
      .map(_.hcursor.downFields("revokeGroupConstraintWaiver", "waiver").focus.get)

  def revokeMutation(wid: Json, reason: Option[String] = None): String =
    val r = reason.fold("")(r => s""", reason: "$r"""")
    s"""
      mutation {
        revokeGroupConstraintWaiver(input: { waiverId: $wid$r }) {
          waiver { $WaiverFields }
        }
      }
    """

  def violationCodes(gid: Group.Id): IO[List[String]] =
    query(pi, s"""query { group(groupId: "$gid") { progress { violations { code } } } }""")
      .map(_.hcursor.downFields("group", "progress", "violations").require[List[Json]].map(_.hcursor.downField("code").require[String]))

  def waivers(gid: Group.Id, includeRevoked: Boolean = false): IO[Json] =
    query(pi, s"""query { group(groupId: "$gid") { waivers(includeRevoked: $includeRevoked) { $WaiverFields } } }""")
      .map(_.hcursor.downFields("group", "waivers").focus.get)

  // An ordered group of two observations, the second completed first, so out
  // of order.
  val outOfOrder: IO[(Group.Id, Observation.Id, Observation.Id)] =
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2))
      _  <- query(pi, s"""mutation { updateGroups(input: { SET: { ordered: true }, WHERE: { id: { EQ: "$g" } } }) { groups { id } } }""")
      _  <- completeObservation(o2)
    yield (g, o1, o2)

  def nightOf(oid: Observation.Id): IO[LocalDate] =
    query(pi, s"""query { observation(observationId: "$oid") { execution { timestamps { completionTime } } } }""")
      .map(_.hcursor.downFields("observation", "execution", "timestamps", "completionTime").require[Timestamp])
      .map(c => LocalObservingNight.fromSiteAndInstant(Site.GN, c.toInstant).toLocalDate)

  test("a PI waives a violation, which no longer counts"):
    for
      (g, _, o2) <- outOfOrder
      w          <- waive(pi, g, orderAt(o2), Some("Usable anyway"))
      vs         <- violationCodes(g)
    yield
      assertEquals(
        w,
        json"""
          {
            "id": ${w.hcursor.downField("id").focus.get},
            "groupId": $g,
            "violation": {
              "code": "ORDER",
              "element": { "groupId": null, "observationId": $o2 },
              "night": null,
              "subgroupId": null
            },
            "granted": { "by": ${pi.id}, "at": ${w.hcursor.downFields("granted", "at").focus.get}, "reason": "Usable anyway" },
            "revoked": null
          }
        """
      )
      w.hcursor.downFields("granted", "at").require[Timestamp]
      assertEquals(vs, Nil)

  test("a waiver is the same in the group's waivers and its progress"):
    for
      (g, _, o2) <- outOfOrder
      _          <- waive(pi, g, orderAt(o2), Some("Usable anyway"))
      js         <- query(pi, s"""query { group(groupId: "$g") { progress { status waived { $WaiverFields } } waivers { $WaiverFields } } }""")
    yield
      val c = js.hcursor.downField("group")
      assertEquals(c.downFields("progress", "status").require[String], "ONGOING")
      assertEquals(c.downFields("progress", "waived").require[List[Json]].length, 1)
      assertEquals(c.downFields("progress", "waived").focus, c.downField("waivers").focus)

  test("staff may waive a violation"):
    for
      (g, _, o2) <- outOfOrder
      w          <- waive(staff, g, orderAt(o2))
    yield
      assertEquals(w.hcursor.downFields("granted", "by").require[User.Id], staff.id)
      assertEquals(w.hcursor.downFields("granted", "reason").focus, Some(Json.Null))

  test("a PI of another program may not waive a violation"):
    outOfOrder.flatMap: (g, _, o2) =>
      expect(pi2, waiveMutation(g, orderAt(o2)), Left(List(s"Group $g doesn't exist, or you may not edit it.")))

  test("a violation that hasn't happened can't be waived"):
    outOfOrder.flatMap: (g, o1, _) =>
      expect(pi, waiveMutation(g, orderAt(o1)), Left(List(s"Group $g has no ORDER violation at $o1 to waive.")))

  test("a violation can't be waived twice"):
    for
      (g, _, o2) <- outOfOrder
      _          <- waive(pi, g, orderAt(o2))
      _          <- expect(pi, waiveMutation(g, orderAt(o2)), Left(List(s"Group $g's ORDER violation at $o2 is already waived.")))
    yield ()

  test("a kind of violation that can't be waived is rejected"):
    outOfOrder.flatMap: (g, _, _) =>
      expect(
        pi,
        waiveMutation(g, "{ code: INSUFFICIENT_ELEMENTS }"),
        Left(List("Argument 'input.violation' is invalid: INSUFFICIENT_ELEMENTS violations can't be waived."))
      )

  test("a kind of violation that can't be waived is reported along with a bad location"):
    outOfOrder.flatMap: (g, _, _) =>
      expect(
        pi,
        waiveMutation(g, """{ code: INSUFFICIENT_ELEMENTS, element: { observationId: "not-an-id" } }"""),
        Left(List(
          "Argument 'input.violation' is invalid: INSUFFICIENT_ELEMENTS violations can't be waived.",
          "Argument 'input.violation.element.observationId' is invalid: 'not-an-id' is not a valid observation id"
        ))
      )

  test("a violation's location must suit its kind"):
    outOfOrder.flatMap: (g, _, o2) =>
      expect(
        pi,
        waiveMutation(g, s"""{ code: ORDER, element: { observationId: "$o2" }, night: { site: GN, date: "2026-01-01" } }"""),
        Left(List("Argument 'input.violation' is invalid: ORDER violations are identified by their element, without a night."))
      ) *>
      expect(
        pi,
        waiveMutation(g, s"""{ code: SAME_NIGHT, element: { observationId: "$o2" } }"""),
        Left(List("Argument 'input.violation' is invalid: SAME_NIGHT violations are identified by their night, without an element."))
      )

  test("a violation on a night is waived by its night"):
    for
      p  <- createProgram
      o1 <- newObservation(p)
      o2 <- newObservation(p)
      g  <- group(p, List(o1, o2))
      _  <- query(pi, s"""mutation { updateGroups(input: { SET: { maximumObservationsPerNight: 1 }, WHERE: { id: { EQ: "$g" } } }) { groups { id } } }""")
      _  <- completeObservation(o1)
      _  <- completeObservation(o2)
      d  <- nightOf(o1)
      w  <- waive(pi, g, s"""{ code: OBSERVATIONS_PER_NIGHT_MAXIMUM, night: { site: GN, date: "$d" } }""")
      vs <- violationCodes(g)
    yield
      assertEquals(
        w.hcursor.downField("violation").focus.get,
        json"""
          {
            "code": "OBSERVATIONS_PER_NIGHT_MAXIMUM",
            "element": null,
            "night": { "site": "GN", "date": ${d.toString} },
            "subgroupId": null
          }
        """
      )
      assertEquals(vs, Nil)

  test("a revoked waiver is kept but no longer applies, and the violation may be waived again"):
    for
      (g, _, o2) <- outOfOrder
      w          <- waive(pi, g, orderAt(o2))
      wid         = w.hcursor.downField("id").focus.get
      r          <- revoke(pi, wid, Some("Not usable after all"))
      vs         <- violationCodes(g)
      active     <- waivers(g)
      all        <- waivers(g, includeRevoked = true)
      _          <- expect(pi, revokeMutation(wid), Left(List(s"Waiver ${wid.asString.get} has already been revoked.")))
      w2         <- waive(pi, g, orderAt(o2))
      after      <- waivers(g, includeRevoked = true)
    yield
      assertEquals(r.hcursor.downField("id").focus, Some(wid))
      assertEquals(r.hcursor.downFields("revoked", "by").require[User.Id], pi.id)
      assertEquals(r.hcursor.downFields("revoked", "reason").require[String], "Not usable after all")
      r.hcursor.downFields("revoked", "at").require[Timestamp]
      assertEquals(vs, List("ORDER"))
      assertEquals(active, json"""[]""")
      assertEquals(all, List(r).asJson)
      assertEquals(after, List(r, w2).asJson)

  test("a PI of another program may not revoke a waiver"):
    for
      (g, _, o2) <- outOfOrder
      w          <- waive(pi, g, orderAt(o2))
      wid         = w.hcursor.downField("id").focus.get
      _          <- expect(pi2, revokeMutation(wid), Left(List(s"Waiver ${wid.asString.get} doesn't exist, or you may not edit its group.")))
    yield ()

  // As with its other properties, only the service may waive a system group's
  // violations or revoke its waivers.
  test("only the service may waive a system group's violation or revoke its waiver"):
    for
      (g, _, o2) <- outOfOrder
      w          <- waive(pi, g, orderAt(o2))
      wid         = w.hcursor.downField("id").focus.get
      _          <- withSession(_.execute(sql"UPDATE t_group SET c_system = true WHERE c_group_id = $group_id".command)(g))
      _          <- expect(pi, revokeMutation(wid), Left(List(s"Waiver ${wid.asString.get} doesn't exist, or you may not edit its group.")))
      _          <- expect(staff, revokeMutation(wid), Left(List(s"Waiver ${wid.asString.get} doesn't exist, or you may not edit its group.")))
      _          <- revoke(serviceUser, wid)
      _          <- expect(pi, waiveMutation(g, orderAt(o2)), Left(List(s"Group $g doesn't exist, or you may not edit it.")))
      _          <- expect(staff, waiveMutation(g, orderAt(o2)), Left(List(s"Group $g doesn't exist, or you may not edit it.")))
      w2         <- waive(serviceUser, g, orderAt(o2))
    yield assertEquals(w2.hcursor.downFields("granted", "by").require[User.Id], serviceUser.id)

  test("granting or revoking a waiver edits its group, for group edit subscribers"):
    for
      (g, _, o2) <- outOfOrder
      _          <- subscriptionExpect(
                      user      = pi,
                      query     = s"""subscription { groupEdit(input: { groupId: "$g" }) { editType value { id } } }""",
                      mutations = Right(
                        for
                          w <- waive(pi, g, orderAt(o2))
                          _ <- IO.sleep(300.millis)
                          _ <- revoke(pi, w.hcursor.downField("id").focus.get)
                          _ <- IO.sleep(300.millis)
                        yield ()
                      ),
                      expected  = List.fill(2)(json"""{ "groupEdit": { "editType": "UPDATED", "value": { "id": $g } } }""")
                    )
    yield ()

  def waiverCount(gid: Group.Id): IO[Long] =
    withSession(_.unique(sql"SELECT COUNT(*) FROM t_group_constraint_waiver WHERE c_group_id = $group_id".query(int8))(gid))

  // The waivers are written directly, there being no way through the API to
  // waive a violation that hasn't happened, and the observations have no
  // targets, which would keep them from being deleted.
  test("deleting a waived element deletes its waivers, and deleting a group deletes the group's"):
    for
      p  <- createProgram
      o1 <- createObservationAs(pi, p)
      o2 <- createObservationAs(pi, p)
      g  <- group(p, List(o1, o2))
      _  <- withSession(_.execute(
              sql"""
                INSERT INTO t_group_constraint_waiver (c_group_id, c_code, c_element_observation_id, c_night_site, c_night_date, c_granted_by)
                VALUES ($group_id, 'order',      $observation_id, NULL, NULL,         $user_id),
                       ($group_id, 'order',      $observation_id, NULL, NULL,         $user_id),
                       ($group_id, 'same_night', NULL,            'gn', '2026-10-16', $user_id)
              """.command
            )((g, o1, pi.id, g, o2, pi.id, g, pi.id)))
      n0 <- waiverCount(g)
      // The last first, so the group's elements stay numbered without gaps.
      _  <- withSession(_.execute(sql"DELETE FROM t_observation WHERE c_observation_id = $observation_id".command)(o2))
      n1 <- waiverCount(g)
      _  <- withSession(_.execute(sql"DELETE FROM t_observation WHERE c_observation_id = $observation_id".command)(o1))
      n2 <- waiverCount(g)
      _  <- withSession(_.execute(sql"DELETE FROM t_group WHERE c_group_id = $group_id".command)(g))
      n3 <- waiverCount(g)
    yield assertEquals((n0, n1, n2, n3), (3L, 2L, 1L, 0L))

