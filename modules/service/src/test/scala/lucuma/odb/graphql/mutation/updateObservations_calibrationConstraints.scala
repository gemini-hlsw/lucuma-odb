// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

class updateObservations_calibrationConstraints
  extends ExecutionTestSupportForGmos
  with TelluricCalibrationsTestSupport:

  val when: Instant = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  // Image quality the calibrations service gives standards and twilights.
  val DefaultIq: String = "TWO_POINT_ZERO"

  val admin: User = TestUsers.Standard.admin(5, 34)

  override val validUsers: List[User] =
    List(pi, pi2, serviceUser, staff, admin)

  case class Setup(pid: Program.Id, science: Observation.Id, spec: Observation.Id, twilight: Observation.Id)

  // The ids the mutation updated, and the messages of any warnings.
  case class Outcome(updated: Set[Observation.Id], warnings: List[String])

  // A GMOS North long slit science observation with its standard and twilight.
  private def setup: IO[Setup] =
    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _    <- runObscalcUpdate(pid, oid)
      _    <- recalculateCalibrations(pid, when, oid)
      spec <- queryCalibrationsWithRole(pid, CalibrationRole.SpectroPhotometric).map(_.head)
      twi  <- queryCalibrationsWithRole(pid, CalibrationRole.Twilight).map(_.head)
    yield Setup(pid, oid, spec, twi)

  private def update(user: User, oids: List[Observation.Id], set: String): IO[Outcome] =
    queryIor(
      user,
      s"""
        mutation {
          updateObservations(input: {
            SET: $set
            WHERE: { id: { IN: ${oids.map(o => s"\"$o\"").mkString("[", ", ", "]")} } }
          }) {
            observations { id }
          }
        }
      """
    ).flatMap: ior =>
      val warnings = ior.left.toList.flatMap(_.toList.map(_.message))
      ior.right.liftTo[IO](new RuntimeException(warnings.mkString("; "))).flatMap: c =>
        c.hcursor.downFields("updateObservations", "observations").as[List[Json]]
          .flatMap(_.traverse(_.hcursor.downField("id").as[Observation.Id]))
          .map(ids => Outcome(ids.toSet, warnings))
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

  private def setPointEight(user: User, oids: Observation.Id*): IO[Outcome] =
    update(user, oids.toList, "{ constraintSet: { imageQuality: POINT_EIGHT } }")

  private def imageQuality(oid: Observation.Id): IO[String] =
    query(
      serviceUser,
      s"""query { observation(observationId: "$oid") { constraintSet { imageQuality } } }"""
    ).flatMap: c =>
      c.hcursor.downFields("observation", "constraintSet", "imageQuality").as[String]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  private def assertReadOnlyWarning(o: Outcome, oid: Observation.Id): Unit =
    assertEquals(o.updated, Set.empty)
    assert(o.warnings.exists(_.contains(s"Calibration observation $oid is read-only")), o.warnings)

  List(staff, admin, serviceUser).foreach: user =>
    test(s"${user.role.access} may edit a standard's conditions"):
      for
        s  <- setup
        o  <- setPointEight(user, s.spec)
        iq <- imageQuality(s.spec)
      yield
        assertEquals(o, Outcome(Set(s.spec), Nil))
        assertEquals(iq, "POINT_EIGHT")

  test("a staff edit that also selects science observations skips the standard"):
    for
      s      <- setup
      o      <- setPointEight(staff, s.science, s.spec)
      specIq <- imageQuality(s.spec)
    yield
      assertEquals(o, Outcome(Set(s.science), Nil))
      assertEquals(specIq, DefaultIq)

  test("a PI edit to a standard is skipped with a warning"):
    for
      s  <- setup
      o  <- setPointEight(pi, s.spec)
      iq <- imageQuality(s.spec)
    yield
      assertReadOnlyWarning(o, s.spec)
      assertEquals(iq, DefaultIq)

  test("a staff edit to a standard's conditions and other fields is skipped with a warning"):
    for
      s  <- setup
      o  <- update(staff, List(s.spec), """{ subtitle: "renamed", constraintSet: { imageQuality: POINT_EIGHT } }""")
      iq <- imageQuality(s.spec)
    yield
      assertReadOnlyWarning(o, s.spec)
      assertEquals(iq, DefaultIq)

  test("staff may not edit a twilight's conditions"):
    for
      s  <- setup
      o  <- setPointEight(staff, s.twilight)
      iq <- imageQuality(s.twilight)
    yield
      assertReadOnlyWarning(o, s.twilight)
      assertEquals(iq, DefaultIq)

  test("a staff edit survives a calibrations re-sync"):
    for
      s     <- setup
      _     <- setPointEight(staff, s.spec)
      _     <- recalculateCalibrations(s.pid, when, s.science)
      specs <- queryCalibrationsWithRole(s.pid, CalibrationRole.SpectroPhotometric)
      iq    <- imageQuality(s.spec)
    yield
      assertEquals(specs, List(s.spec))
      assertEquals(iq, "POINT_EIGHT")
