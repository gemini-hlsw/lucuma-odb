// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import fs2.Stream
import fs2.text.utf8
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.itc.client.SpectroscopyInput
import lucuma.odb.graphql.query.ExecutionTestSupportForGnirs
import lucuma.odb.graphql.query.GaiaVoTables
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gnirs
import org.http4s.Request
import org.http4s.Response
import skunk.Session

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

class calibrations_gnirs_altair
  extends OdbSuite
  with ExecutionTestSupportForGnirs
  with ObservingModeSetupOperations
  with TelluricCalibrationsTestSupport:

  val when: Instant = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  // Behind Altair the sequence is generated for the guide star the observation will actually use,
  // so these observations need candidates inside the AOWFS patrol field to select one from.
  override protected def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    _ => Resource.eval(IO.pure(Response(body = Stream(GaiaVoTables.altairCandidates).through(utf8.encode))))

  override def fakeItcSpectroscopyResultFor(input: SpectroscopyInput): Option[IntegrationTime] =
    input.parameters.mode.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(time, count, _) => IntegrationTime(time, count).some
      case ExposureTimeMode.SignalToNoiseMode(_, _)          => IntegrationTime(5.minuteTimeSpan, PosInt.unsafeFrom(4)).some

  // The shared GNIRS support seeds the D111 / MIRROR keys; the cross-dispersed
  // observation below needs its own D32 / SXD science rows.
  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    val key: Gnirs.TableKey =
      Gnirs.TableKey(
        GnirsPixelScale.PixelScale_0_15,
        GnirsGrating.D32,
        GnirsPrism.Sxd,
        BoundedInterval.unsafeOpenUpper(
          Wavelength.fromIntNanometers(900).get,
          Wavelength.fromIntNanometers(2560).get
        ),
        GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
        GnirsWellDepth.Shallow
      )
    // The daytime pinhole flat looks up the same config with the pinhole FPU.
    val pinholeKey: Gnirs.TableKey =
      key.copy(fpu = GnirsFpu.Other(GnirsFpuOther.Pinhole3))
    val rows: List[Gnirs.TableRow] =
      List(
        Gnirs.TableRow(PosLong.unsafeFrom(1), key, gnirsSmartFlat),
        Gnirs.TableRow(PosLong.unsafeFrom(1), key, gnirsSmartArc),
        Gnirs.TableRow(PosLong.unsafeFrom(1), pinholeKey, gnirsSmartFlat)
      )
    // Ids are offset past those the shared support inserts.
    super.dbInitialization.fold(IO.unit)(_(s)) >>
      servicesFor(pi).map(_(s)).use: services =>
        services.transactionally:
          rows.zipWithIndex.traverse_ : (row, index) =>
            Services.asSuperUser:
              services.smartGcalService.insertGnirs(1000 + index, row)

  private def altairJson(mode: String, fieldLens: Option[String], cassRotator: String, ndFilter: String): Json =
    json"""
      {
        "mode": $mode,
        "explicitFieldLens": $fieldLens,
        "cassRotator": $cassRotator,
        "ndFilter": $ndFilter
      }
    """

  private def setAltair(oid: Observation.Id, altair: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { targetEnvironment: { altair: $altair } }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  // The laser requires cloud cover 50% or better and image quality 70% or
  // better; without them the observation is not Defined and gets no
  // calibrations at all.  Harmless for the NGS observations, which share it.
  private def setConditionsGoodEnoughForTheLaser(oid: Observation.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { constraintSet: { imageQuality: POINT_ONE, cloudExtinction: ZERO } }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def queryAltair(oid: Observation.Id): IO[Json] =
    query(
      serviceUser,
      s"""
        query {
          observation(observationId: "$oid") {
            targetEnvironment {
              altair { mode explicitFieldLens cassRotator ndFilter }
            }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "targetEnvironment", "altair").focus.getOrElse(Json.Null))

  private def calibrationObservationFor(oid: Observation.Id, role: CalibrationRole): IO[Option[Observation.Id]] =
    queryObservation(oid).flatMap: obs =>
      obs.groupId.flatTraverse: gid =>
        queryObservationsInGroup(gid).map: infos =>
          infos.find(_.calibrationRole.exists(_ === role)).map(_.id)

  // A cross-dispersed (SXD prism) GNIRS observation.
  private def createGnirsXdObservationAs(user: User, pid: Program.Id, tid: Target.Id): IO[Observation.Id] =
    query(
      user = user,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson},
            SET: {
              targetEnvironment: { asterism: ${List(tid).asJson} }
              scienceRequirements: ${scienceRequirementsObject(ObservingModeType.GnirsLongSlit)}
              observingMode: {
                gnirsSpectroscopy: {
                  grating: D32
                  prism: SXD
                  camera: SHORT_BLUE
                  slit: { fpu: LONG_SLIT_0_30 }
                  filter: ORDER3
                  centralWavelengths: [
                    {
                      centralWavelength: { nanometers: 1650 }
                      exposureTimeMode: { timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 1650 } } }
                    }
                  ]
                }
              }
              constraintSet: { imageQuality: POINT_EIGHT }
            }
          }) { observation { id } }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  // NGS and LGS need a guide star to be configured at all, and an observation with a configuration
  // error gets no calibrations.
  private val GuideStarName: String =
    "Gaia DR3 3219118090462918016"

  private def scienceWithAltair(altair: Option[String]): IO[(Program.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGnirsLongSlitObservationAs(pi, pid, tid)
      _   <- setConditionsGoodEnoughForTheLaser(oid)
      _   <- altair.traverse_(setAltair(oid, _))
      _   <- setGuideTargetName(pi, oid, GuideStarName.some)
      _   <- runObscalcUpdate(pid, oid)
      _   <- recalculateCalibrations(pid, when, oid)
    yield (pid, oid)

  test("an LGS science observation gives its telluric NGS with the field lens in"):
    for
      (_, oid) <- scienceWithAltair("{ mode: LGS }".some)
      telOpt   <- calibrationObservationFor(oid, CalibrationRole.Telluric)
      altair   <- telOpt.traverse(queryAltair)
    yield
      assert(telOpt.isDefined, "expected a telluric calibration")
      assertEquals(altair, altairJson("NGS", "IN".some, "FOLLOWING", "OUT").some)

  test("an NGS science observation passes its field lens and cass rotator to its telluric"):
    for
      (_, oid) <- scienceWithAltair("{ mode: NGS, fieldLens: OUT, cassRotator: FIXED, ndFilter: IN }".some)
      telOpt   <- calibrationObservationFor(oid, CalibrationRole.Telluric)
      altair   <- telOpt.traverse(queryAltair)
    yield
      assert(telOpt.isDefined, "expected a telluric calibration")
      // The ND filter depends on the standard's own guide star, so it is always out.
      assertEquals(altair, altairJson("NGS", "OUT".some, "FIXED", "OUT").some)

  test("clearing the science Altair configuration clears the telluric's"):
    for
      (pid, oid) <- scienceWithAltair("{ mode: NGS }".some)
      telOpt     <- calibrationObservationFor(oid, CalibrationRole.Telluric)
      before     <- telOpt.traverse(queryAltair)
      _          <- setAltair(oid, "null")
      _          <- recalculateCalibrations(pid, when, oid)
      after      <- telOpt.traverse(queryAltair)
    yield
      assertEquals(before, altairJson("NGS", none, "FOLLOWING", "OUT").some)
      assertEquals(after, Json.Null.some)

  test("a science observation without Altair gives its telluric none"):
    for
      (_, oid) <- scienceWithAltair(none)
      telOpt   <- calibrationObservationFor(oid, CalibrationRole.Telluric)
      altair   <- telOpt.traverse(queryAltair)
    yield
      assert(telOpt.isDefined, "expected a telluric calibration")
      assertEquals(altair, Json.Null.some)

  test("a daytime pinhole flat never observes behind Altair"):
    for
      pid    <- createProgramAs(pi)
      tid    <- createTargetWithProfileAs(pi, pid)
      oid    <- createGnirsXdObservationAs(pi, pid, tid)
      _      <- setConditionsGoodEnoughForTheLaser(oid)
      _      <- setAltair(oid, "{ mode: LGS }")
      _      <- setGuideTargetName(pi, oid, GuideStarName.some)
      _      <- runObscalcUpdate(pid, oid)
      _      <- recalculateCalibrations(pid, when, oid)
      pinOpt <- calibrationObservationFor(oid, CalibrationRole.DaytimePinhole)
      telOpt <- calibrationObservationFor(oid, CalibrationRole.Telluric)
      pin    <- pinOpt.traverse(queryAltair)
      tel    <- telOpt.traverse(queryAltair)
    yield
      assert(pinOpt.isDefined, "expected a daytime pinhole calibration")
      assertEquals(pin, Json.Null.some)
      assertEquals(tel, altairJson("NGS", "IN".some, "FOLLOWING", "OUT").some)
