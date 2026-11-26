// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.Existence
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2

import java.time.LocalDateTime
import java.time.ZoneOffset

// Cloning and cascading existence changes through a user group that holds a
// science observation inside its per-observation calibration group.
class groupCalibrationGroups
  extends ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport:

  private val when =
    LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  private case class Elem(
    parent:    Option[Group.Id],
    existence: Existence,
    group:     Option[Group.Id],
    obs:       Option[Observation.Id],
    role:      Option[String]
  )

  private def allElements(gid: Group.Id, includeDeleted: Boolean = true): IO[List[Elem]] =
    query(
      pi,
      s"""query { group(groupId: "$gid") { allElements(includeDeleted: $includeDeleted) { parentGroupId existence group { id } observation { id calibrationRole } } } }"""
    ).map:
      _.hcursor.downFields("group", "allElements").require[List[Json]].map: e =>
        val c: io.circe.HCursor = e.hcursor
        Elem(
          c.downField("parentGroupId").require[Option[Group.Id]],
          c.downField("existence").require[Existence],
          c.downFields("group", "id").as[Group.Id].toOption,
          c.downFields("observation", "id").as[Observation.Id].toOption,
          c.downFields("observation", "calibrationRole").as[Option[String]].toOption.flatten
        )

  private def groupParent(gid: Group.Id): IO[Option[Group.Id]] =
    query(pi, s"""query { group(groupId: "$gid") { parentId } }""").map:
      _.hcursor.downFields("group", "parentId").require[Option[Group.Id]]

  private def groupExistence(gid: Group.Id): IO[Existence] =
    query(pi, s"""query { group(groupId: "$gid") { existence } }""").map:
      _.hcursor.downFields("group", "existence").require[Existence]

  private def cascade(gid: Group.Id, existence: String): IO[Json] =
    query(
      pi,
      s"""mutation { updateGroups(input: { SET: { existence: $existence }, WHERE: { id: { EQ: "$gid" } }, cascade: true }) { groups { id } } }"""
    )

  // User group U holding a science observation, which sits in its calibration group.
  private def scienceInUserGroup: IO[(Program.Id, Group.Id, Observation.Id, Group.Id)] =
    for
      pid <- createProgramAs(pi)
      u   <- createGroupAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _   <- moveObservationAs(pi, oid, Some(u))
      _   <- setExposureTime(oid, 120)
      _   <- recalculateCalibrations(pid, when, oid)
      obs <- queryObservation(oid)
      cg   = obs.groupId.get
      par <- groupParent(cg)
    yield
      assertNotEquals(cg, u)
      assertEquals(par, Some(u))
      (pid, u, oid, cg)

  test("cloning a group clones only the science observation of a calibration group inside it"):
    for
      (_, u, oid, cg) <- scienceInUserGroup
      clone           <- cloneGroupAs(pi, u)
      elems           <- allElements(clone)
      inCal           <- queryObservationsInGroup(cg)
      sci             <- queryObservation(oid)
    yield
      assertEquals(elems.length, 1)
      assertEquals(elems.head.parent, Some(clone))
      assertEquals(elems.head.role, None)
      assert(elems.head.obs.exists(_ =!= oid))
      assertEquals(sci.groupId, Some(cg))
      assert(inCal.exists(_.id === oid))
      assert(inCal.exists(_.calibrationRole.isDefined))

  test("the clone of a science observation gets its own calibration group in the cloned group"):
    for
      (pid, u, _, cg) <- scienceInUserGroup
      clone           <- cloneGroupAs(pi, u)
      elems           <- allElements(clone)
      coid             = elems.head.obs.get
      _               <- recalculateCalibrations(pid, when, coid)
      obs             <- queryObservation(coid)
      cg2              = obs.groupId.get
      par             <- groupParent(cg2)
    yield
      assertNotEquals(cg2, cg)
      assertEquals(par, Some(clone))

  private type Row = (Option[Group.Id], Existence, Option[Group.Id], Option[Observation.Id])

  private def rows(gid: Group.Id, includeDeleted: Boolean = true): IO[Set[Row]] =
    allElements(gid, includeDeleted).map(_.map(e => (e.parent, e.existence, e.group, e.obs)).toSet)

  private def telluricsOf(cg: Group.Id): IO[List[Observation.Id]] =
    queryObservationsInGroup(cg).map(_.filter(_.calibrationRole.isDefined).map(_.id))

  // The science observation is in a calibration group under `u` that holds tellurics.
  private def assertCalibrated(u: Group.Id, oid: Observation.Id): IO[Unit] =
    for
      obs <- queryObservation(oid)
      cg   = obs.groupId.get
      par <- groupParent(cg)
      tel <- telluricsOf(cg)
    yield
      assertEquals(par, Some(u))
      assert(tel.nonEmpty, "no tellurics in the calibration group")

  test("cascade delete leaves the calibration group to the service, which then removes it"):
    for
      (pid, u, oid, cg) <- scienceInUserGroup
      tellurics         <- telluricsOf(cg)
      _                 <- cascade(u, "DELETED")
      uEx               <- groupExistence(u)
      flagged           <- rows(u)
      _                 <- recalculateCalibrations(pid, when, oid)
      exist             <- tellurics.traverse(queryObservationExists)
      after             <- rows(u)
    yield
      assertEquals(uEx, Existence.Deleted)
      assertEquals(
        flagged,
        (Set[Row]((Some(u), Existence.Present, Some(cg), None), (Some(cg), Existence.Deleted, None, Some(oid))) ++
          tellurics.map(t => (Some(cg), Existence.Present, None, Some(t)))))
      assert(tellurics.nonEmpty)
      assert(exist.forall(!_), s"tellurics remain: $exist")
      assertEquals(after, Set[Row]((Some(u), Existence.Deleted, None, Some(oid))))

  test("cascade restore before the service runs brings everything back as it was"):
    for
      (pid, u, oid, cg) <- scienceInUserGroup
      before            <- rows(u)
      _                 <- cascade(u, "DELETED")
      _                 <- cascade(u, "PRESENT")
      uEx               <- groupExistence(u)
      after             <- rows(u)
      _                 <- recalculateCalibrations(pid, when, oid)
      _                 <- assertCalibrated(u, oid)
    yield
      assertEquals(uEx, Existence.Present)
      assertEquals(after, before)
      assert(before.nonEmpty)

  test("cascade restore after the service ran gives the science observation a calibration group again"):
    for
      (pid, u, oid, _) <- scienceInUserGroup
      _                <- cascade(u, "DELETED")
      _                <- recalculateCalibrations(pid, when, oid)
      _                <- cascade(u, "PRESENT")
      uEx              <- groupExistence(u)
      direct           <- rows(u)
      _                <- recalculateCalibrations(pid, when, oid)
      _                <- assertCalibrated(u, oid)
    yield
      assertEquals(uEx, Existence.Present)
      assertEquals(direct, Set[Row]((Some(u), Existence.Present, None, Some(oid))))

  // Asserts that nothing changed: the groups, the science observation and every telluric are as they were.
  private def assertUntouched(u: Group.Id, cg: Group.Id, oid: Observation.Id, inCal: List[Observation.Id]): IO[Unit] =
    for
      uEx   <- groupExistence(u)
      cgEx  <- groupExistence(cg)
      sci   <- queryObservation(oid)
      after <- queryObservationsInGroup(cg).map(_.map(_.id))
      exist <- inCal.traverse(queryObservationExists)
    yield
      assertEquals(uEx, Existence.Present)
      assertEquals(cgEx, Existence.Present)
      assertEquals(sci.groupId, Some(cg))
      assertEquals(after.toSet, inCal.toSet)
      assert(exist.forall(identity), s"a telluric no longer exists: $exist")

  test("cascade delete with a protected telluric fails naming it and changes nothing"):
    for
      (_, u, oid, cg) <- scienceInUserGroup
      inCal           <- queryObservationsInGroup(cg).map(_.map(_.id))
      tellurics       <- queryObservationsInGroup(cg).map(_.filter(_.calibrationRole.isDefined).map(_.id))
      _               <- declareExecutionComplete(tellurics.head) // a completed telluric is protected from deletion
      protectedOid     = tellurics.head
      _               <- expectOdbError(
                           user     = pi,
                           query    = s"""mutation { updateGroups(input: { SET: { existence: DELETED }, WHERE: { id: { EQ: "$u" } }, cascade: true }) { groups { id } } }""",
                           expected = { case OdbError.InvalidArgument(Some(msg)) if msg.contains(protectedOid.toString) => () }
                         )
      _               <- assertUntouched(u, cg, oid, inCal)
    yield assert(tellurics.length > 1)

  test("a normal user's cascade delete matching a calibration group changes nothing"):
    for
      (_, u, oid, cg) <- scienceInUserGroup
      inCal           <- queryObservationsInGroup(cg).map(_.map(_.id))
      r               <- cascade(cg, "DELETED")
      _               <- assertUntouched(u, cg, oid, inCal)
    yield assertEquals(r.hcursor.downFields("updateGroups", "groups").require[List[Json]], Nil)

  test("a delete without cascade still fails while the calibration group holds a present science observation"):
    for
      (_, u, oid, cg) <- scienceInUserGroup
      inCal           <- queryObservationsInGroup(cg).map(_.map(_.id))
      _               <- expectOdbError(
                           user     = pi,
                           query    = s"""mutation { updateGroups(input: { SET: { existence: DELETED }, WHERE: { id: { EQ: "$u" } } }) { groups { id } } }""",
                           expected = { case OdbError.InconsistentGroupError(Some("Deleted group contains non-deleted element(s).")) => () }
                         )
      _               <- assertUntouched(u, cg, oid, inCal)
    yield ()

  test("restoring only the science observation of a deleted group is rejected"):
    for
      (_, u, oid, _) <- scienceInUserGroup
      _              <- cascade(u, "DELETED")
      before         <- rows(u)
      _              <- expectOdbError(
                          user     = pi,
                          query    = s"""mutation { updateObservations(input: { SET: { existence: PRESENT }, WHERE: { id: { EQ: "$oid" } }, includeDeleted: true }) { observations { id } } }""",
                          expected = { case OdbError.InconsistentGroupError(Some("Deleted group contains non-deleted element(s).")) => () }
                        )
      after          <- rows(u)
    yield assertEquals(after, before)
