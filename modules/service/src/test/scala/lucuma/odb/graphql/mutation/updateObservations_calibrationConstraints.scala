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

  case class Setup(pid: Program.Id, science: Observation.Id, spec: Observation.Id, twilight: Observation.Id)

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

  // Returns the ids the mutation actually updated.
  private def update(user: User, oids: List[Observation.Id], set: String): IO[Set[Observation.Id]] =
    query(
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
    ).flatMap: c =>
      c.hcursor.downFields("updateObservations", "observations").as[List[Json]]
        .flatMap(_.traverse(_.hcursor.downField("id").as[Observation.Id]))
        .map(_.toSet)
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  private def setPointEight(user: User, oids: Observation.Id*): IO[Set[Observation.Id]] =
    update(user, oids.toList, "{ constraintSet: { imageQuality: POINT_EIGHT } }")

  private def imageQuality(oid: Observation.Id): IO[String] =
    query(
      serviceUser,
      s"""query { observation(observationId: "$oid") { constraintSet { imageQuality } } }"""
    ).flatMap: c =>
      c.hcursor.downFields("observation", "constraintSet", "imageQuality").as[String]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  test("staff may edit a standard's conditions alongside science observations"):
    for
      s       <- setup
      updated <- setPointEight(staff, s.science, s.spec)
      sciIq   <- imageQuality(s.science)
      specIq  <- imageQuality(s.spec)
    yield
      assertEquals(updated, Set(s.science, s.spec))
      assertEquals(sciIq, "POINT_EIGHT")
      assertEquals(specIq, "POINT_EIGHT")

  test("a PI edit skips the standard and updates the science observations"):
    for
      s       <- setup
      updated <- setPointEight(pi, s.science, s.spec)
      specIq  <- imageQuality(s.spec)
    yield
      assertEquals(updated, Set(s.science))
      assertEquals(specIq, DefaultIq)

  test("a staff edit to a standard's conditions and other fields skips the standard"):
    for
      s       <- setup
      updated <- update(staff, List(s.spec), """{ subtitle: "renamed", constraintSet: { imageQuality: POINT_EIGHT } }""")
      iq      <- imageQuality(s.spec)
    yield
      assertEquals(updated, Set.empty)
      assertEquals(iq, DefaultIq)

  test("staff may not edit a twilight's conditions"):
    for
      s       <- setup
      updated <- setPointEight(staff, s.twilight)
      iq      <- imageQuality(s.twilight)
    yield
      assertEquals(updated, Set.empty)
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
