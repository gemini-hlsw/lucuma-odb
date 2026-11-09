// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Json
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.PerProgramPerConfigCalibrationsService
import lucuma.odb.util.Codecs.observation_id
import skunk.codec.all.text
import skunk.syntax.all.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

class calibrations_gmosSpecPhotDaytimeArc
  extends ExecutionTestSupportForGmos
  with TelluricCalibrationsTestSupport:

  val when: Instant = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  case class ArcAtom(description: String, observeClass: String, steps: List[(String, String)])

  private val atomQuery: String =
    """
      description
      observeClass
      steps {
        stepConfig { stepType }
        observeClass
      }
    """

  private def decodeAtom(j: Json): Either[io.circe.DecodingFailure, ArcAtom] =
    val c = j.hcursor
    for
      d  <- c.downField("description").as[String]
      o  <- c.downField("observeClass").as[String]
      ss <- c.downField("steps").as[List[Json]]
      st <- ss.traverse: s =>
              (s.hcursor.downFields("stepConfig", "stepType").as[String], s.hcursor.downField("observeClass").as[String]).tupled
    yield ArcAtom(d, o, st)

  private def queryScienceAtoms(oid: Observation.Id): IO[List[ArcAtom]] =
    query(pi, executionConfigQuery(oid, "gmosNorth", "science", atomQuery, 100.some)).flatMap: c =>
      val sci = c.hcursor.downFields("executionConfig", "gmosNorth", "science")
      (for
        next   <- sci.downField("nextAtom").as[Json]
        future <- sci.downField("possibleFuture").as[List[Json]]
        atoms  <- (next :: future).traverse(decodeAtom)
      yield atoms).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  private def hasAcquisitionAtom(oid: Observation.Id): IO[Boolean] =
    query(pi, executionConfigQuery(oid, "gmosNorth", "acquisition", "observeClass", none)).map:
      _.hcursor.downFields("executionConfig", "gmosNorth", "acquisition", "nextAtom").focus.exists(!_.isNull)

  private def queryObscalcState(oid: Observation.Id): IO[String] =
    session.use(_.unique(sql"SELECT c_obscalc_state::text FROM t_obscalc WHERE c_observation_id = $observation_id".query(text))(oid))

  case class GroupInfo(name: String, calibrationRoles: List[CalibrationRole], parentId: Option[Group.Id], system: Boolean) derives Decoder

  private def queryGroupInfo(gid: Group.Id): IO[GroupInfo] =
    query(
      serviceUser,
      s"""query {
            group(groupId: "$gid") {
              name
              calibrationRoles
              parentId
              system
            }
          }"""
    ).flatMap: c =>
      c.hcursor.downField("group").as[GroupInfo].leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  private def queryAsterismIds(oid: Observation.Id): IO[List[Json]] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              targetEnvironment { asterism { id } }
            }
          }"""
    ).map(_.hcursor.downFields("observation", "targetEnvironment", "asterism").as[List[Json]].getOrElse(Nil))

  private def queryCalibrationsWithRole(pid: Program.Id, calibrationRole: CalibrationRole): IO[List[Observation.Id]] =
    query(
      serviceUser,
      s"""query {
            observations(WHERE: { program: { id: { EQ: "$pid" } }, calibrationRole: { EQ: ${calibrationRole.tag.toUpperCase} } }) {
              matches { id }
            }
          }"""
    ).flatMap: c =>
      c.hcursor.downFields("observations", "matches").as[List[Json]]
        .flatMap(_.traverse(_.hcursor.downField("id").as[Observation.Id]))
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  private def queryLongSlitMode(oid: Observation.Id): IO[Json] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              observingMode {
                gmosNorthLongSlit {
                  grating
                  filter
                  fpu
                  centralWavelength { nanometers }
                  wavelengthDithers { nanometers }
                }
              }
            }
          }"""
    ).map(_.hcursor.downFields("observation", "observingMode", "gmosNorthLongSlit").focus.getOrElse(Json.Null))

  private def queryWorkflowState(oid: Observation.Id): IO[ObservationWorkflowState] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              workflow { value { state } }
            }
          }"""
    ).flatMap: c =>
      c.hcursor.downFields("observation", "workflow", "value", "state").as[ObservationWorkflowState]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  private def setCentralWavelength(oid: Observation.Id, nm: Int): IO[Unit] =
    query(
      pi,
      s"""mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: { observingMode: { gmosNorthLongSlit: { centralWavelength: { nanometers: $nm } } } }
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

  // A GMOS North long slit science observation after a calibration pass, with
  // its spectrophotometric standard and the standard's daytime arc.
  private def setup: IO[(Program.Id, Observation.Id, Observation.Id, Observation.Id)] =
    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _    <- runObscalcUpdate(pid, oid)
      _    <- recalculateCalibrations(pid, when, oid)
      spec <- queryCalibrationsWithRole(pid, CalibrationRole.SpectroPhotometric).map(_.head)
      arc  <- calibrationObservationFor(spec, CalibrationRole.DaytimeArc).map(_.get)
    yield (pid, oid, spec, arc)

  test("a spectrophotometric standard gets a daytime arc in a group nested in the Calibrations group"):
    for
      (_, _, spec, arc) <- setup
      specObs           <- queryObservation(spec)
      arcObs            <- queryObservation(arc)
      grp               <- queryGroupInfo(specObs.groupId.get)
      parent            <- queryGroupInfo(grp.parentId.get)
      asterism          <- queryAsterismIds(arc)
    yield
      assertEquals(grp.name, s"gmos_north_long_slit/calibration/$spec")
      assertEquals(grp.calibrationRoles, List(CalibrationRole.DaytimeArc))
      assert(grp.system)
      assertEquals(parent.name, PerProgramPerConfigCalibrationsService.CalibrationsGroupName.value)
      assertEquals(arcObs.groupId, specObs.groupId)
      assertEquals(arcObs.calibrationRole, Some(CalibrationRole.DaytimeArc))
      assertEquals(asterism, Nil)

  test("daytime arc carries the standard's observing mode"):
    for
      (_, _, spec, arc) <- setup
      specMode          <- queryLongSlitMode(spec)
      arcMode           <- queryLongSlitMode(arc)
    yield
      assert(!specMode.isNull)
      assertEquals(arcMode, specMode)

  test("a second calibration pass keeps the one daytime arc"):
    for
      (pid, oid, spec, arc) <- setup
      (added, removed)      <- recalculateCalibrations(pid, when, oid)
      arcs                  <- calibrationObservationsFor(spec, CalibrationRole.DaytimeArc)
    yield
      assertEquals(added, Nil)
      assertEquals(removed, Nil)
      assertEquals(arcs, List(arc))

  test("daytime arc sequence is one smart gcal arc atom, with no acquisition"):
    for
      (_, _, _, arc) <- setup
      atoms          <- queryScienceAtoms(arc)
      acq            <- hasAcquisitionAtom(arc)
    yield
      assertEquals(atoms, List(ArcAtom("Daytime Arc", "DAY_CAL", List(("GCAL", "DAY_CAL")))))
      assert(!acq, "a daytime arc has no acquisition")

  test("daytime arc workflow follows its standard"):
    for
      (pid, _, spec, arc) <- setup
      _                   <- runObscalcUpdate(pid, spec)
      _                   <- runObscalcUpdate(pid, arc)
      before              <- queryWorkflowState(arc)
      specState           <- setObservationWorkflowState(pi, spec, ObservationWorkflowState.Inactive)
      _                   <- runObscalcUpdate(pid, arc)
      after               <- queryWorkflowState(arc)
    yield
      assertEquals(before, ObservationWorkflowState.Defined)
      assertEquals(specState, ObservationWorkflowState.Inactive)
      assertEquals(after, ObservationWorkflowState.Inactive)

  test("daytime arc and group are removed with the standard when the science is deleted"):
    for
      (pid, oid, spec, arc) <- setup
      gid                   <- queryObservation(spec).map(_.groupId.get)
      _                     <- deleteObservation(pi, oid)
      (_, removed)          <- recalculateCalibrations(pid, when, oid)
      specExists            <- queryObservationExists(spec)
      arcExists             <- queryObservationExists(arc)
      groupExists           <- queryGroupExists(gid)
    yield
      assert(removed.contains(spec), s"the standard should be removed: $removed")
      assert(!specExists, "the standard should be deleted")
      assert(!arcExists, "the daytime arc should be deleted")
      assert(!groupExists, "the standard's group should be deleted")

  test("a science configuration change replaces the standard and its arc"):
    for
      (pid, oid, spec, arc) <- setup
      _                     <- setCentralWavelength(oid, 600)
      _                     <- runObscalcUpdate(pid, oid)
      _                     <- recalculateCalibrations(pid, when, oid)
      specExists            <- queryObservationExists(spec)
      arcExists             <- queryObservationExists(arc)
      specs                 <- queryCalibrationsWithRole(pid, CalibrationRole.SpectroPhotometric)
      arcs                  <- specs.flatTraverse(calibrationObservationsFor(_, CalibrationRole.DaytimeArc))
    yield
      assert(!specExists, "the old standard should be deleted")
      assert(!arcExists, "the old daytime arc should be deleted")
      assertEquals(specs.size, 1)
      assertEquals(arcs.size, 1)

  test("a daytime arc with a visit keeps its standard and group"):
    for
      (pid, oid, spec, arc) <- setup
      gid                   <- queryObservation(spec).map(_.groupId.get)
      _                     <- recordVisitAs(serviceUser, arc)
      _                     <- deleteObservation(pi, oid)
      (_, removed)          <- recalculateCalibrations(pid, when, oid)
      specExists            <- queryObservationExists(spec)
      arcExists             <- queryObservationExists(arc)
      groupExists           <- queryGroupExists(gid)
    yield
      assert(!removed.contains(spec) && !removed.contains(arc), s"neither the standard nor its arc should be removed: $removed")
      assert(specExists, "a standard whose arc was visited must be kept")
      assert(arcExists, "a visited daytime arc must be kept")
      assert(groupExists, "the group holding them must be kept")

  test("daytime arc goes pending when the standard's mode is re-synced"):
    for
      (pid, oid, spec, arc) <- setup
      _                     <- runObscalcUpdate(pid, arc)
      before                <- queryObscalcState(arc)
      _                     <- recalculateCalibrations(pid, when, oid)
      after                 <- queryObscalcState(arc)
    yield
      assertEquals(before, "ready")
      assertEquals(after, "pending")

  test("neither pi nor staff can edit the daytime arc"):
    for
      (_, _, _, arc) <- setup
      byPi           <- updateSubtitleAs(pi, arc)
      byStaff        <- updateSubtitleAs(staff, arc)
    yield
      assertEquals(byPi, Nil)
      assertEquals(byStaff, Nil)

  test("daytime arc cannot be declined"):
    for
      (_, _, _, arc) <- setup
      result         <- setObservationWorkflowState(pi, arc, ObservationWorkflowState.Inactive).attempt
    yield assert(result.isLeft, "a daytime arc must not be declinable")
