// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.Json
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import lucuma.odb.util.Codecs.observation_id
import skunk.Session
import skunk.codec.all.text
import skunk.syntax.all.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

class calibrations_f2ImagingDaytimeFlat
  extends OdbSuite
  with ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport:

  val when: Instant = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  // Imaging day flats, lamp on and off, as in Flamingos2_FLAT.csv.  K-long has
  // no row, there or here.
  private def imagingDayFlat(shutter: GcalShutter): SmartGcalValue[LegacyInstrumentConfig] =
    SmartGcalValue(
      Gcal(
        Gcal.Lamp.fromContinuum(GcalContinuum.IrGreyBodyHigh),
        GcalFilter.Nd45,
        GcalDiffuser.Ir,
        shutter
      ),
      GcalBaselineType.Day,
      PosInt.unsafeFrom(1),
      LegacyInstrumentConfig(TimeSpan.unsafeFromMicroseconds(3_000_000L))
    )

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    val rows: List[Flamingos2.TableRow] =
      for
        filter  <- List(Flamingos2Filter.Y, Flamingos2Filter.J, Flamingos2Filter.H)
        shutter <- List(GcalShutter.Open, GcalShutter.Closed)
      yield Flamingos2.TableRow(PosLong.unsafeFrom(1), Flamingos2.TableKey(none, filter, none), imagingDayFlat(shutter))

    super.dbInitialization.traverse_(_(s)) *>
      servicesFor(pi).map(_(s)).use: services =>
        services.transactionally:
          rows.zipWithIndex.traverse_ : (r, i) =>
            Services.asSuperUser:
              services.smartGcalService.insertFlamingos2(100 + i, r)

  case class FlatAtom(description: String, observeClass: String, steps: List[(String, String)])

  private val atomQuery: String =
    """
      description
      observeClass
      steps {
        stepConfig { stepType }
        observeClass
      }
    """

  private def decodeAtom(j: Json): Either[io.circe.DecodingFailure, FlatAtom] =
    val c = j.hcursor
    for
      d  <- c.downField("description").as[String]
      o  <- c.downField("observeClass").as[String]
      ss <- c.downField("steps").as[List[Json]]
      st <- ss.traverse: s =>
              (s.hcursor.downFields("stepConfig", "stepType").as[String], s.hcursor.downField("observeClass").as[String]).tupled
    yield FlatAtom(d, o, st)

  private def queryScienceAtoms(oid: Observation.Id): IO[List[FlatAtom]] =
    query(pi, executionConfigQuery(oid, "flamingos2", "science", atomQuery, 100.some)).flatMap: c =>
      val sci = c.hcursor.downFields("executionConfig", "flamingos2", "science")
      (for
        next   <- sci.downField("nextAtom").as[Json]
        future <- sci.downField("possibleFuture").as[List[Json]]
        atoms  <- (next :: future).traverse(decodeAtom)
      yield atoms).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  private def hasScienceAtom(oid: Observation.Id): IO[Boolean] =
    query(pi, executionConfigQuery(oid, "flamingos2", "science", "observeClass", none)).map:
      _.hcursor.downFields("executionConfig", "flamingos2", "science", "nextAtom").focus.exists(!_.isNull)

  private def queryObscalcState(oid: Observation.Id): IO[String] =
    session.use(_.unique(sql"SELECT c_obscalc_state::text FROM t_obscalc WHERE c_observation_id = $observation_id".query(text))(oid))

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

  // Filters by their GraphQL enum names, e.g. K_LONG.
  private def setImagingFilters(oid: Observation.Id, filters: List[String]): IO[Unit] =
    val fs = filters.map(f => s"{ filter: $f }").mkString("[", ", ", "]")
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
      _                <- setImagingFilters(oid, List("H", "J"))
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

  private val dayFlatSteps: List[(String, String)] =
    List(("GCAL", "DAY_CAL"), ("GCAL", "DAY_CAL"))

  test("daytime flat sequence is one smart gcal flat atom per filter"):
    for
      (_, oid, flat) <- setup
      filters        <- queryImagingFilters(flat)
      atoms          <- queryScienceAtoms(flat)
    yield assertEquals(atoms, filters.map(f => FlatAtom(s"Daytime Flat $f", "DAY_CAL", dayFlatSteps)))

  test("daytime flat of a one-filter science observation has a bare title"):
    for
      (pid, oid, flat) <- setup
      _                <- setImagingFilters(oid, List("H"))
      _                <- recalculateCalibrations(pid, when, oid)
      atoms            <- queryScienceAtoms(flat)
    yield assertEquals(atoms, List(FlatAtom("Daytime Flat", "DAY_CAL", dayFlatSteps)))

  test("daytime flat goes pending when a science filter edit is copied to it"):
    for
      (pid, oid, flat) <- setup
      _                <- runObscalcUpdate(pid, flat)
      before           <- queryObscalcState(flat)
      _                <- setImagingFilters(oid, List("H"))
      _                <- recalculateCalibrations(pid, when, oid)
      after            <- queryObscalcState(flat)
    yield
      assertEquals(before, "ready")
      assertEquals(after, "pending")

  test("a filter without a smart gcal row fails only the daytime flat sequence"):
    for
      (pid, oid, flat) <- setup
      _                <- setImagingFilters(oid, List("K_LONG"))
      _                <- recalculateCalibrations(pid, when, oid)
      flatResult       <- queryScienceAtoms(flat).attempt
      sciAtom          <- hasScienceAtom(oid)
    yield
      assert(flatResult.left.exists(_.getMessage.contains(s"Could not generate a sequence for $flat: missing Smart GCAL mapping")), flatResult.toString)
      assert(sciAtom, "the science sequence still generates")

  test("daytime flat with a visit survives the science leaving f2 imaging"):
    for
      (pid, oid, flat) <- setup
      gid              <- queryObservation(oid).map(_.groupId.get)
      _                <- recordVisitAs(serviceUser, flat)
      _                <- switchToGmosNorthLongSlit(oid)
      (_, removed)     <- recalculateCalibrations(pid, when, oid)
      flatExists       <- queryObservationExists(flat)
      groupExists      <- queryGroupExists(gid)
    yield
      assertEquals(removed, Nil)
      assert(flatExists, "a visited daytime flat must be kept")
      assert(groupExists, "the group holding it must be kept")
