// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Json
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

class calibrations_f2ImagingDaytimeFlat
  extends OdbSuite
  with ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport:

  val when: Instant = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  case class GroupNameAndRoles(name: String, calibrationRoles: List[CalibrationRole]) derives Decoder

  private def queryGroupNameAndRoles(gid: Group.Id): IO[GroupNameAndRoles] =
    query(
      serviceUser,
      s"""query {
            group(groupId: "$gid") {
              name
              calibrationRoles
            }
          }"""
    ).flatMap: c =>
      c.hcursor.downField("group").as[GroupNameAndRoles].leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  private def queryAsterismIds(oid: Observation.Id): IO[List[Json]] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              targetEnvironment { asterism { id } }
            }
          }"""
    ).map(_.hcursor.downFields("observation", "targetEnvironment", "asterism").as[List[Json]].getOrElse(Nil))

  private def queryImagingFilters(oid: Observation.Id): IO[List[String]] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              observingMode {
                flamingos2Imaging {
                  filters { filter }
                }
              }
            }
          }"""
    ).flatMap: c =>
      c.hcursor
        .downFields("observation", "observingMode", "flamingos2Imaging", "filters")
        .as[List[Json]]
        .flatMap(_.traverse(_.hcursor.downField("filter").as[String]))
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  private def setImagingFilters(oid: Observation.Id, filters: List[Flamingos2Filter]): IO[Unit] =
    val fs = filters.map(f => s"{ filter: ${f.tag.toUpperCase} }").mkString("[", ", ", "]")
    query(
      pi,
      s"""mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: { observingMode: { flamingos2Imaging: { filters: $fs } } }
            }) {
              observations { id }
            }
          }"""
    ).void

  private def switchToGmosNorthLongSlit(oid: Observation.Id): IO[Unit] =
    query(
      pi,
      s"""mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                  }
                }
              }
            }) {
              observations { id }
            }
          }"""
    ).void

  private def updateSubtitleAs(user: User, oid: Observation.Id): IO[List[Json]] =
    query(
      user,
      s"""mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: { subtitle: "edited" }
            }) {
              observations { id }
            }
          }"""
    ).map(_.hcursor.downFields("updateObservations", "observations").as[List[Json]].getOrElse(Nil))

  // An F2 imaging science observation after a calibration pass, with its flat.
  private def setup: IO[(Program.Id, Observation.Id, Observation.Id)] =
    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createFlamingos2ImagingObservationAs(pi, pid, tid)
      _    <- runObscalcUpdate(pid, oid)
      _    <- recalculateCalibrations(pid, when, oid)
      flat <- calibrationObservationFor(oid, CalibrationRole.DaytimeFlat).map(_.get)
    yield (pid, oid, flat)

  test("f2 imaging observation gets a daytime flat in its own calibration group"):
    for
      (_, oid, flat) <- setup
      sci            <- queryObservation(oid)
      flatObs        <- queryObservation(flat)
      grp            <- queryGroupNameAndRoles(sci.groupId.get)
      tellurics      <- calibrationObservationsFor(oid, CalibrationRole.Telluric)
      asterism       <- queryAsterismIds(flat)
    yield
      assertEquals(grp.name, s"flamingos_2_imaging/calibration/$oid")
      assertEquals(grp.calibrationRoles, List(CalibrationRole.DaytimeFlat))
      assertEquals(flatObs.groupId, sci.groupId)
      assertEquals(flatObs.calibrationRole, Some(CalibrationRole.DaytimeFlat))
      assertEquals(tellurics, Nil)
      assertEquals(asterism, Nil)

  test("daytime flat carries the science filters"):
    for
      (_, oid, flat) <- setup
      filters        <- queryImagingFilters(flat)
    yield assertEquals(filters, List("Y", "J"))

  test("a second calibration pass keeps the one daytime flat"):
    for
      (pid, oid, flat) <- setup
      (added, removed) <- recalculateCalibrations(pid, when, oid)
      flats            <- calibrationObservationsFor(oid, CalibrationRole.DaytimeFlat)
    yield
      assertEquals(added, Nil)
      assertEquals(removed, Nil)
      assertEquals(flats, List(flat))

  test("a science filter edit is copied to the daytime flat"):
    for
      (pid, oid, flat) <- setup
      _                <- setImagingFilters(oid, List(Flamingos2Filter.H, Flamingos2Filter.J))
      _                <- runObscalcUpdate(pid, oid)
      _                <- recalculateCalibrations(pid, when, oid)
      sciFilters       <- queryImagingFilters(oid)
      filters          <- queryImagingFilters(flat)
    yield
      assertEquals(filters.toSet, Set("H", "J"))
      assertEquals(filters, sciFilters)

  test("daytime flat and group are removed when the science leaves f2 imaging"):
    for
      (pid, oid, flat) <- setup
      gid              <- queryObservation(oid).map(_.groupId.get)
      _                <- switchToGmosNorthLongSlit(oid)
      (_, removed)     <- recalculateCalibrations(pid, when, oid)
      flatExists       <- queryObservationExists(flat)
      groupExists      <- queryGroupExists(gid)
    yield
      assertEquals(removed, List(flat))
      assert(!flatExists, "daytime flat should be deleted")
      assert(!groupExists, "calibration group should be deleted")

  test("daytime flat and group are removed when the science is deleted"):
    for
      (pid, oid, flat) <- setup
      gid              <- queryObservation(oid).map(_.groupId.get)
      _                <- deleteObservation(pi, oid)
      _                <- recalculateCalibrations(pid, when, oid)
      flatExists       <- queryObservationExists(flat)
      groupExists      <- queryGroupExists(gid)
    yield
      assert(!flatExists, "daytime flat should be deleted")
      assert(!groupExists, "calibration group should be deleted")

  test("neither pi nor staff can edit the daytime flat"):
    for
      (_, _, flat) <- setup
      byPi         <- updateSubtitleAs(pi, flat)
      byStaff      <- updateSubtitleAs(staff, flat)
    yield
      assertEquals(byPi, Nil)
      assertEquals(byStaff, Nil)

  test("daytime flat cannot be declined"):
    for
      (_, _, flat) <- setup
      result       <- setObservationWorkflowState(pi, flat, ObservationWorkflowState.Inactive).attempt
    yield assert(result.isLeft, "a daytime flat must not be declinable")
