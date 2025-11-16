// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.feature

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.odb.data.EditType
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.graphql.query.ExecutionQuerySetupOperations
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.graphql.subscription.SubscriptionUtils
import lucuma.odb.json.wavelength.decoder.given
import lucuma.odb.service.CalibrationsService
import lucuma.odb.service.PerProgramPerConfigCalibrationsService
import lucuma.odb.service.Services
import lucuma.odb.service.SpecPhotoCalibrations
import lucuma.odb.service.TwilightCalibrations

import skunk.implicits.*
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.*

class perProgramPerConfigCalibrations
    extends OdbSuite
    with SubscriptionUtils
    with ExecutionQuerySetupOperations
    with ExecutionTestSupport {
  override val pi       = TestUsers.Standard.pi(1, 101)
  val service  = TestUsers.service(3)

  val DefaultSnAt: Wavelength = Wavelength.fromIntNanometers(510).get

  override val validUsers = List(pi, service)

  def updateTargetProperties(pi: User, tid: Target.Id, ra: Long, dec: Long, rv:  Double): IO[Json] =
    query(
      pi,
      s"""
          mutation {
            updateTargets(input: {
              SET: {
                sidereal: {
                  ra: { degrees: ${Angle.fromMicroarcseconds(ra).toDoubleDegrees} }
                  dec: { degrees: ${Angle.fromMicroarcseconds(dec).toDoubleDegrees} }
                  radialVelocity: { metersPerSecond: $rv }
                  epoch: "J2000.000"
                  radialVelocity: {
                    kilometersPerSecond: 0.0
                  }
                },
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: B5_III
                      },
                      brightnesses: [
                        {
                          band: R
                          value: 15.0
                          units: VEGA_MAGNITUDE
                        }
                      ]
                    }
                  }
                }
              }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
              }
            }
          }
      """
    )

  def scienceRequirements(pi: User, oid: Observation.Id, snAt: Wavelength = DefaultSnAt): IO[Json] =
    query(
      pi,
      s"""
          mutation {
            updateObservations(input: {
              SET: {
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 75.000,
                      at: { nanometers: ${snAt.toNanometers} }
                    }
                  },
                  spectroscopy: {
                    wavelength: {
                      nanometers: 400.000
                    },
                    resolution: 10,
                    wavelengthCoverage: {
                      nanometers: 0.010
                    },
                    focalPlane: SINGLE_SLIT,
                    focalPlaneAngle: {
                      arcseconds: 5
                    }
                  }
                }
              }
              WHERE: {
                id: { EQ: "$oid"}
              }
            }) {
              observations {
                id
              }
            }
          }
      """
    )

  def prepareObservation(pi: User, oid: Observation.Id, tid: Target.Id, snAt: Wavelength = DefaultSnAt): IO[Unit] =
    for {
      _ <- updateTargetProperties(
             pi,
             tid,
             RightAscension.Zero.toAngle.toMicroarcseconds,
             Declination.Zero.toAngle.toMicroarcseconds,
             0.0
           )
      _ <- scienceRequirements(pi, oid, snAt)
    } yield ()



  val when = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  // Utility classes used to decode group queries
  case class CalibTarget(id: Target.Id) derives Decoder
  case class CalibTE(firstScienceTarget: Option[CalibTarget]) derives Eq, Decoder
  case class CalibCE(cloudExtinction: CloudExtinction.Preset) derives Decoder
  case class ExposureTimeMode(signalToNoise: SignalToNoise) derives Decoder
  case class ScienceRequirements(exposureTimeMode: ExposureTimeMode) derives Decoder
  case class SignalToNoise(at: Wavelength) derives Decoder
  case class GmosNorthLongSlit(explicitRoi: Option[GmosRoi]) derives Decoder
  case class GmosSouthLongSlit(explicitRoi: Option[GmosRoi]) derives Decoder
  case class ObservingMode(gmosNorthLongSlit: Option[GmosNorthLongSlit], gmosSouthLongSlit: Option[GmosSouthLongSlit]) derives Decoder
  case class CalibObs(
    id: Observation.Id,
    groupId: Option[Group.Id],
    calibrationRole: Option[CalibrationRole],
    targetEnvironment: Option[CalibTE],
    constraintSet: Option[CalibCE],
    scienceRequirements: ScienceRequirements,
    observingMode: Option[ObservingMode]
  ) derives Decoder

  extension (obs: List[CalibObs])
    def countCalibrations: Int = obs.count:
      case CalibObs(_, _, Some(_), _, _, _, _) => true
      case _                               => false

    def countCalibrationsWithCE(ce: CloudExtinction.Preset): Int = obs.count:
      case CalibObs(_, _, Some(_), _, Some(CalibCE(a)), _, _) => a === ce
      case _                                               => false

    def countCalibrationsWithTE: Int = obs.count:
      case CalibObs(_, _, Some(_), Some(_), _, _, _) => true
      case _                                     => false

    def callibrationIds: List[Observation.Id] = obs.collect:
      case CalibObs(cid, _, Some(_), _, _, _, _) => cid

    def groupIds: List[Group.Id] = obs.collect:
      case CalibObs(_, Some(gid), _, _, _, _, _) => gid

  extension (elements: List[Either[Group.Id, Observation.Id]])
    def observationIds: List[Observation.Id] = elements.collect:
      case Right(oid) => oid

    def calibrationGroupId: Option[Group.Id] = elements.collectFirst:
      case Left(gid) => gid

  private def queryGroup(gid: Group.Id): IO[(Group.Id, Boolean, NonEmptyString)] =
    query(
      service,
      s"""query { group(groupId: "$gid") { id system name} }"""
    ).flatMap { c =>
      (for {
        id    <- c.hcursor.downField("group").downField("id").as[Group.Id]
        sys   <- c.hcursor.downField("group").downField("system").as[Boolean]
        name  <- c.hcursor.downField("group").downField("name").as[NonEmptyString]
      } yield (id, sys, name))
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
     }

  private def queryObservations(pid: Program.Id): IO[List[CalibObs]] =
    query(
      service,
      s"""query {
            observations(
              WHERE: {
                program: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }) {
                matches {
                  id
                  groupId
                  calibrationRole
                  constraintSet {
                    cloudExtinction
                  }
                  scienceRequirements {
                    exposureTimeMode {
                      signalToNoise {
                        at { nanometers }
                      }
                    }
                  }
                  observingMode {
                    gmosNorthLongSlit {
                      explicitRoi
                    }
                    gmosSouthLongSlit {
                      explicitRoi
                    }
                  }
                  targetEnvironment {
                    firstScienceTarget {
                      id
                      sidereal {
                        ra {
                          degrees
                        }
                        dec {
                          degrees
                        }
                      }
                    }
                  }
                }
              }
            }"""
    ).flatMap { c =>
      (for {
        id    <- c.hcursor.downField("observations").downField("matches").as[List[CalibObs]]
      } yield id)
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
     }

  def formatLD(ld: LocalDate): String = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")
    ld.atStartOfDay().atOffset(ZoneOffset.UTC).format(formatter)
  }

  test("no calibrations group if not needed") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, None)
      _   <- recalculateCalibrations(pid, when)
      gr1 <- groupElementsAs(pi, pid, None)
    } yield {
      val cgid = gr1.calibrationGroupId
      assert(cgid.isEmpty)
    }
  }

  test("create group for calibrations") {
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "One")
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      _   <- prepareObservation(pi, oid, tid)
      gr  <- groupElementsAs(pi, pid, None)
      _   <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      cgid = gr1.calibrationGroupId
      cg   <- cgid.map(queryGroup)
                .getOrElse(IO.raiseError(new RuntimeException("No calibration group")))
      ob   <- queryObservations(pid)
    } yield {
      assertEquals(gr.size, 1)
      assert(cg._2)
      assertEquals(cg._3, PerProgramPerConfigCalibrationsService.CalibrationsGroupName)
    }
  }

  test("add calibrations for each LongSlit mode") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.observationIds
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrations
      // calibs belong to the calib group
      val obsGids = ob.groupIds
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 4)
      assertEquals(oids.size, 2)
    }
  }

  test("distinguish read mode") {
    for
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")

      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- updateConf(oid1, GmosAmpReadMode.Fast, GmosAmpGain.High)

      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- updateConf(oid2, GmosAmpReadMode.Fast, GmosAmpGain.High)

      oid3 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      _    <- updateConf(oid3, GmosAmpReadMode.Slow, GmosAmpGain.Low)

      oid4 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      _    <- updateConf(oid4, GmosAmpReadMode.Slow, GmosAmpGain.Low)

      _    <- prepareObservation(pi, oid1, tid1) *>
              prepareObservation(pi, oid2, tid1) *>
              prepareObservation(pi, oid3, tid2) *>
              prepareObservation(pi, oid4, tid2)

      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    yield
      val oids = gr1.observationIds
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrations
      // calibs belong to the calib group
      val obsGids = ob.groupIds
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 4)  // one group of 2 for each of (oid1, oid2) and (oid3, oid4)
      assertEquals(oids.size, 4)
  }

  test("specphoto cloud extinction") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.observationIds
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrationsWithCE(CloudExtinction.Preset.ThreePointZero)
      // calibs belong to the calib group
      val obsGids = ob.groupIds
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 1)
      assertEquals(oids.size, 1)
    }
  }

  test("twilight cloud extinction") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.observationIds
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrationsWithCE(CloudExtinction.Preset.PointThree)
      // calibs belong to the calib group
      val obsGids = ob.groupIds
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 1)
      assertEquals(oids.size, 1)
    }
  }

  test("add calibrations for each LongSlit mode ignoring ones without conf") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createIncompleteTargetAs(pi, pid, "One")
      tid2 <- createIncompleteTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
              // No target for oid2 -> no conf
      _    <- prepareObservation(pi, oid1, tid1) *> scienceRequirements(pi, oid2)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.observationIds
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrations
      // calibs belong to the calib group
      val obsGids = ob.groupIds
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 2)
      assertEquals(oids.size, 2)
    }
  }

  def calibrationTargets(role: CalibrationRole, referenceInstant: Instant) =
    withServices(pi) { services =>
      services.calibrationsService.calibrationTargets(List(role), referenceInstant)
    }

  test("calculate best target for specphoto") {

    val samples: List[(Site, Instant, String)] = List(
        (Site.GN, Instant.parse("2024-08-28T07:00:00Z"), "HD 340611"),
        (Site.GN, Instant.parse("2024-08-28T10:00:00Z"), "BD+28  4211"),
        (Site.GS, Instant.parse("2024-08-28T04:00:00Z"), "LP  877-23"),
        (Site.GS, Instant.parse("2024-08-28T07:00:00Z"), "CD-28   595"),
      )

    for {
      tgts <- calibrationTargets(CalibrationRole.SpectroPhotometric, when)
      _    <- samples.traverse { case (site, instant, name) =>
                val id = SpecPhotoCalibrations.bestTarget(site, instant, tgts)
                tgts.find(_._1 === id.map(_._2).get).map(_._2).fold(
                  IO.raiseError(new RuntimeException(s"Target $id not found"))
                )(n => assertIOBoolean(IO(n === name)))
              }
    } yield ()
  }

  test("calculate best target for twilight") {

    val samples: List[(Site, Instant, String, Coordinates)] = List(
        // after midnight
        (Site.GN, Instant.parse("2024-08-28T10:00:00Z"), "Twilight", Coordinates.fromHmsDms.getOption("19:31:27.840000 +22:20:38.400000").get),
        // before midnight
        (Site.GN, Instant.parse("2024-08-24T01:00:00Z"), "Twilight", Coordinates.fromHmsDms.getOption("15:31:05.760000 +26:15:17.600000").get),
        // before midnight
        (Site.GS, Instant.parse("2024-08-28T03:00:00Z"), "Twilight", Coordinates.fromHmsDms.getOption("22:07:12.720000 -19:14:43.400000").get),
        // after midnight
        (Site.GS, Instant.parse("2024-08-24T10:00:00Z"), "Twilight", Coordinates.fromHmsDms.getOption("01:39:26.570000 -32:50:56.800000").get),
      )

    for {
      tgts <- calibrationTargets(CalibrationRole.Twilight, when)
      _    <- samples.traverse { case (site, instant, name, coord) =>
                val id = TwilightCalibrations.bestTarget(site, instant, tgts)
                tgts.find(_._1 === id.map(_._2).get).fold(
                  IO.raiseError(new RuntimeException(s"Target $id not found"))
                )((_, n, _, c) => assertIOBoolean(IO(n === name && c == coord)))
              }
    } yield ()
  }

  test("select calibration target") {
    for {
      tpid <- withServices(service) { s =>
                Services.asSuperUser:
                  s.session.transaction.use { xa =>
                      s.programService
                        .insertCalibrationProgram(
                          ProgramPropertiesInput.Create.Default.some,
                          CalibrationRole.SpectroPhotometric,
                          Description.unsafeFrom("SPECTROTEST"))(using xa)
                }
              }
      // PI program
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      _    <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1).flatTap { oid =>
                prepareObservation(pi, oid, tid1)
              }
      _    <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2).flatTap { oid =>
                prepareObservation(pi, oid, tid2)
              }
      _    <- recalculateCalibrations(pid, when)
      ob   <- queryObservations(pid)
    } yield {
      val cCount = ob.countCalibrationsWithTE
      assertEquals(cCount, 4)
    }
  }

  test("add calibrations is idempotent") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- recalculateCalibrations(pid, when)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.observationIds
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrations
      // calibs belong to the calib group
      val obsGids = ob.groupIds
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 4)
      assertEquals(oids.size, 2)
    }
  }

  test("add calibrations is idempotent when an obs has no conf") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createIncompleteTargetAs(pi, pid, "One")
      tid2 <- createIncompleteTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> scienceRequirements(pi, oid2)
      _    <- recalculateCalibrations(pid, when)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.observationIds
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrations
      // calibs belong to the calib group
      val obsGids = ob.groupIds
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 2)
      assertEquals(oids.size, 2)
    }
  }

  test("calibration observations can't be modified by the pi") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      _    <- recalculateCalibrations(pid, when)
      ob   <- queryObservations(pid)
      cid = ob.callibrationIds
      _    <- expect(
                user = pi,
                query = s"""
                  mutation {
                    updateObservations(input: {
                      SET: {
                        constraintSet: {
                          cloudExtinction: ONE_POINT_ZERO
                        }
                      },
                      WHERE: {
                        id: { EQ: "${cid.get(0).get}" }
                      }
                    }) {
                      observations {
                        constraintSet {
                          cloudExtinction
                        }
                      }
                    }
                  }
                """,
                expected =json"""
                  {
                    "updateObservations": {
                      "observations": []
                    }
                  }
                """.asRight
              )
    } yield ()
  }

  test("calibration observations obs time can be changed") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      _    <- recalculateCalibrations(pid, when)
      ob   <- queryObservations(pid)
      cid = ob.callibrationIds
      _    <- expect(
                user = pi,
                query = s"""
                  mutation {
                    updateObservationsTimes(input: {
                      SET: {
                        observationTime: "2011-12-03T10:15:30Z"
                      },
                      WHERE: {
                        id: { EQ: "${cid.get(0).get}" }
                      }
                    }) {
                      observations {
                        observationTime
                      }
                    }
                  }
                """,
                expected =json"""
                  {
                    "updateObservationsTimes": {
                      "observations": [ {
                          "observationTime": "2011-12-03 10:15:30"
                        }
                      ]
                    }
                  }
                """.asRight
              )
    } yield ()
  }

  test("calibration observations obs time can switch target") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      _    <- recalculateCalibrations(pid, when)
      ob   <- queryObservations(pid)
      (cid1, ct1) = ob.collect {
        case CalibObs(cid, _, Some(_), Some(ct1), _, _, _) => (cid, ct1)
      }.head
      _    <- query(
                user = pi,
                query = s"""
                  mutation {
                    updateObservationsTimes(input: {
                      SET: {
                        observationTime: "2026-08-15T01:15:30Z"
                      },
                      WHERE: {
                        id: { EQ: "$cid1" }
                      }
                    }) {
                      observations {
                        observationTime
                      }
                    }
                  }
                """
              )
      // In reality this is done listening to events but we can explicitly call the function here
      _     <- withServices(service) { services =>
                 services.session.transaction.use { xa =>
                   Services.asSuperUser:
                     services.calibrationsService.recalculateCalibrationTarget(pid, cid1)(using xa)
                 }
               }
      ob2   <- queryObservations(pid)
      (cid2, ct2) = ob2.collect {
        case CalibObs(cid, _, Some(_), Some(ct2), _, _, _) => (cid, ct2)
      }.head
      // Some observation
      _     <- assertIOBoolean(IO(ob2.map(_.id) === ob.map(_.id)))
      // Target changed
      _     <- assertIOBoolean(IO(ct1 =!= ct2 && cid1 === cid2))
    } yield ()
  }

  test("calibration observations can't be cloned") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      _    <- recalculateCalibrations(pid, when)
      ob   <- queryObservations(pid)
      cid = ob.callibrationIds.head
      _    <- expect(
                user = pi,
                query = s"""
                  mutation {
                    cloneObservation(input: {
                      observationId: "$cid"
                    }) {
                      originalObservation {
                        id
                      }
                      newObservation {
                        id
                      }
                    }
                  }
                """,
                expected = List(s"User ${pi.id} is not authorized to perform this operation.").asLeft
              )
    } yield ()
  }

  test("unnecessary calibrations are removed") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- recalculateCalibrations(pid, when)
      _    <- deleteObservation(pi, oid2)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.observationIds
      val cCount = ob.countCalibrations
      // Only two calibration as we removed a config
      assertEquals(cCount, 2)
      assertEquals(oids.size, 1)
    }
  }

  def deletedSubscription(pid: Program.Id) =
    s"""
      subscription {
        observationEdit(input: { programId: "${pid.show}" }) {
          observationId
          editType
          value {
            id
            title
          }
        }
      }
    """

  def calibrationDeleted(oid: Observation.Id): Json =
    Json.obj(
      "observationEdit" -> Json.obj(
        "observationId" -> oid.asJson,
        "editType" -> Json.fromString(EditType.HardDelete.tag.toUpperCase),
        "value"    -> Json.Null
      )
    )

  test("subscription events created when calibrations are calculated") {
    val expectedTargets = List("Feige  34", "Twilight")

    for {
      pid  <- createProgram(pi, "foo")
      tid0 <- createTargetAs(pi, pid, "Zero")
      // An observation with a single target is essentially a calib observation
      oid  <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid0)
      _    <- prepareObservation(pi, oid, tid0)
      a    <- Ref.of[IO, List[Observation.Id]](Nil) // Added observation
      _    <- subscriptionExpectFT(
                user      = pi,
                query     = deletedSubscription(pid),
                mutations =
                  Right(recalculateCalibrations(pid, when).flatMap { case (cids, _) =>
                    a.set(cids)
                  }),
                expectedF =
                  (a.get.map(_.sortBy(_.value.value).zip(expectedTargets).flatMap {
                    case (cid, tn) => List(
                      json"""
                        {
                          "observationEdit" : {
                            "observationId" : $cid,
                            "editType" : "CREATED",
                            "value" : {
                              "id" : $cid,
                              "title": $tn
                            }
                          }
                        }
                      """,
                      json"""
                        {
                          "observationEdit" : {
                            "observationId" : $cid,
                            "editType" : "UPDATED",
                            "value" : {
                              "id" : $cid,
                              "title": $tn
                            }
                          }
                        }
                      """,
                    )
                  })),
                transform = _.sortBy(u =>
                    val oe = u.hcursor.downField("observationEdit")
                    val id = oe.downField("observationId").as[Observation.Id].toOption.map(_.value.value)
                    val et = oe.downField("editType").as[EditType].toOption.map(_.tag)
                    (id, et)
                  )
              )
    } yield ()
  }

  test("events for deleting a calibration observation") {
    for {
      pid  <- createProgram(pi, "foo")
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
              // This will add four calibrations
      (ad, _) <- recalculateCalibrations(pid, when)
              // This should delete two
      _    <- deleteObservation(pi, oid2)
      a    <- Ref.of[IO, List[Observation.Id]](Nil) // Removed observation
      // The third twilight
      rd = ad.get(2).get
      _    <- subscriptionExpectF(
                user      = pi,
                query     = deletedSubscription(pid),
                mutations =
                  Right(recalculateCalibrations(pid, when).flatMap { case (_, cids) =>
                    a.set(cids)
                  }),
                expectedF = (
                  a.get.map(_.map: cid =>
                    json"""
                      {
                        "observationEdit" : {
                          "observationId" : $cid,
                          "editType" : "UPDATED",
                          "value" : null
                        }
                      }
                    """
                  ).map {
                     // A twligiht observation is updated, we'll hardcode this
                     case List(a, b) => a :: json"""{
                                                     "observationEdit" : {
                                                       "observationId" : $rd,
                                                       "editType" : "UPDATED",
                                                       "value" : {
                                                         "id" : $rd,
                                                         "title": "Twilight"
                                                       }
                                                     }
                                                   }
                                                 """ :: b :: Nil
                     case l => l
                  },
                  // N.B. for the deletion events the order isn't guaranteed
                  // because, I think, the DELETE FROM t_observation WHERE c_observation_id IN (...)
                  // can delete the calibrations in any order.  Unfortunately
                  // we expect the events in a particular order though.
                  a.get.map(_.map: cid =>
                    json"""
                      {
                        "observationEdit" : {
                          "observationId" : $cid,
                          "editType" : "HARD_DELETE",
                          "value" : null
                        }
                      }
                    """
                  )
                ).mapN(_ ::: _)
              )
    } yield ()
  }

  test("spec photo signal to noise at is taken from the science obs average") {
    for {
      pid  <- createProgram(pi, "foo")
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1, Wavelength.fromIntNanometers(500).get) *>
                prepareObservation(pi, oid2, tid2, Wavelength.fromIntNanometers(520).get)
              // This will add four calibrations
      (ad, _) <- recalculateCalibrations(pid, when)
      ob   <- queryObservations(pid)
    } yield {
      val wv = ob.collect {
        case CalibObs(_, _, Some(CalibrationRole.SpectroPhotometric), _, _, ScienceRequirements(ExposureTimeMode(SignalToNoise(wv))), _) => wv
      }
      // 510 is the average across the science observations (500 + 520) / 2 = 510
      assertEquals(Wavelength.fromIntNanometers(510), wv.headOption)
    }
  }

  test("Don't add calibrations if science is inactive"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- setObservationWorkflowState(pi, oid1, ObservationWorkflowState.Inactive)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.observationIds
      val cCount = ob.countCalibrations
      assertEquals(cCount, 2)
      assertEquals(oids.size, 2)
    }

  def updateCentralWavelength(oid: Observation.Id, centralWavelength: Wavelength): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gmosNorthLongSlit: {
                  centralWavelength: { nanometers: ${centralWavelength.toNanometers} }
                }
              }
            },
            WHERE: {
              id: { EQ: "$oid" }
            }
          }) {
            observations {
              instrument
            }
          }
        }
      """
    ).void

  def updateConf(oid: Observation.Id, readMode: GmosAmpReadMode, gain: GmosAmpGain): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gmosNorthLongSlit: {
                  explicitAmpReadMode: ${readMode.tag.toScreamingSnakeCase}
                  explicitAmpGain: ${gain.tag.toScreamingSnakeCase}
                }
              }
            },
            WHERE: {
              id: { EQ: "$oid" }
            }
          }) {
            observations {
              instrument
            }
          }
        }
      """
    ).void

  test("unnecessary calibrations are removed unless partially executed"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      // should produce 2 calibrations
      _    <- recalculateCalibrations(pid, when)
      ob1   <- queryObservations(pid)
      calibIds1 = ob1.callibrationIds
      // Add execution events to one of the calibrations (making it partially executed)
      setup = ExecutionQuerySetupOperations.Setup(offset = 0, atomCount = 1, stepCount = 1, datasetCount = 1)
      _     <- recordVisit(ObservingModeType.GmosNorthLongSlit, setup, service, calibIds1.head)
      // Change the observation configuration
      _     <- updateCentralWavelength(oid1, Wavelength.fromIntNanometers(600).get)
      // Run calibrations again - should keep the partially executed calibration and add new ones
      _     <- recalculateCalibrations(pid, when)
      ob2   <- queryObservations(pid)
      calibIds2 = ob2.callibrationIds
      gr1  <- groupElementsAs(pi, pid, None)
      // Verify that the calibration still has execution events
      evs  <- withServices(service) { services =>
                  services.executionEventService.selectSequenceEvents(calibIds1.head).compile.toList
                }
      _     <- assertIOBoolean(IO(evs.nonEmpty))
    } yield {
      val oids = gr1.observationIds
      val count1 = ob1.countCalibrations
      val count2 = ob2.countCalibrations
      // initial set 2 calibs
      assertEquals(count1, 2)
      // After config change: 1 partially executed calibration remains + 2 new calibrations
      assertEquals(count2, 3)
      // The partially executed calibration should still be present
      assert(calibIds2.contains(calibIds1.head))
      assertEquals(oids.size, 1)
    }

  test("SpectroPhotometric calibrations ignore ROI differences from science observations") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "Target1")
      tid2 <- createTargetAs(pi, pid, "Target2")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid1" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: CENTRAL_SPECTRUM
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """,
      )
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid2" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: FULL_FRAME
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """,
      )
      _ <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      // Generate calibrations
      _ <- recalculateCalibrations(pid, when)
      ob <- queryObservations(pid)
    } yield {
      val calibCount = ob.count(_.calibrationRole.contains(CalibrationRole.SpectroPhotometric))
      // 1 Specphoto even though we have two ROI
      assertEquals(calibCount, 1)
    }
  }

  test("Twilight calibrations preserve ROI differences from science observations") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "Target1")
      tid2 <- createTargetAs(pi, pid, "Target2")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid1" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: CENTRAL_SPECTRUM
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid2" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: FULL_FRAME
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """,
      )
      _ <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _ <- recalculateCalibrations(pid, when)

      ob <- queryObservations(pid)
    } yield {
      val twilightCount = ob.count(_.calibrationRole.contains(CalibrationRole.Twilight))
      val twilightObs = ob.filter(_.calibrationRole.contains(CalibrationRole.Twilight))
      val rois = twilightObs.flatMap(_.observingMode).flatMap(_.gmosNorthLongSlit).flatMap(_.explicitRoi)

      // Twilight preserve ROI differences, thus two twilight observations
      assertEquals(twilightCount, 2)
      assertEquals(rois.count(_ === GmosRoi.CentralSpectrum), 1)
      assertEquals(rois.count(_ === GmosRoi.FullFrame), 1)
    }
  }

  test("twilight calibrations are properly removed when science observation is deleted") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "Target1")
      tid2 <- createTargetAs(pi, pid, "Target2")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      // Set different ROIs for the two observations
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid1" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: CENTRAL_SPECTRUM
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid2" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: FULL_FRAME
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- recalculateCalibrations(pid, when)
      obsBefore <- queryObservations(pid)
      _    <- deleteObservation(pi, oid2)  // Delete full frame
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      obsAfter   <- queryObservations(pid)
    } yield {
      // Before deletion: should have 1 specphoto + 2 twilights (one for each ROI)
      val specphotoBefore = obsBefore.count(_.calibrationRole.exists(_ === CalibrationRole.SpectroPhotometric))
      val twilightBefore = obsBefore.count(_.calibrationRole.exists(_ === CalibrationRole.Twilight))
      assertEquals(specphotoBefore, 1)
      assertEquals(twilightBefore, 2)

      // After deletion: one each type
      val specphotoAfter = obsAfter.count(_.calibrationRole.exists(_ === CalibrationRole.SpectroPhotometric))
      val twilightAfter = obsAfter.count(_.calibrationRole.exists(_ === CalibrationRole.Twilight))

      assertEquals(specphotoAfter, 1)
      assertEquals(twilightAfter, 1)

      // Verify only the needed ROI remains in twilight calibrations
      val remainingTwilightRois = obsAfter
        .filter(_.calibrationRole.contains(CalibrationRole.Twilight))
        .flatMap(_.observingMode)
        .flatMap(_.gmosNorthLongSlit)
        .flatMap(_.explicitRoi)
      assertEquals(remainingTwilightRois, List(GmosRoi.CentralSpectrum))
    }
  }

  test("twilight calibrations are properly removed when science observation ROI is converted") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "Target1")
      tid2 <- createTargetAs(pi, pid, "Target2")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      // Set different ROIs for the two observations
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid1" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: CENTRAL_SPECTRUM
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid2" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: FULL_FRAME
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- recalculateCalibrations(pid, when)
      obsBefore <- queryObservations(pid)
      // Convert full observation to central
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid2" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: CENTRAL_SPECTRUM
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      obsAfter   <- queryObservations(pid)
    } yield {
      // Before conversion: 1 specphoto + 2 twilights
      val specphotoBefore = obsBefore.count(_.calibrationRole.exists(_ === CalibrationRole.SpectroPhotometric))
      val twilightBefore = obsBefore.count(_.calibrationRole.exists(_ === CalibrationRole.Twilight))

      assertEquals(specphotoBefore, 1)
      assertEquals(twilightBefore, 2)

      // After conversion: calibrations remain available even when no longer strictly needed
      val specphotoAfter = obsAfter.count(_.calibrationRole.contains(CalibrationRole.SpectroPhotometric))
      val twilightAfter = obsAfter.count(_.calibrationRole.contains(CalibrationRole.Twilight))
      val twilightRoisAfter = obsAfter
        .filter(_.calibrationRole.contains(CalibrationRole.Twilight))
        .flatMap(_.observingMode)
        .flatMap(_.gmosNorthLongSlit)
        .flatMap(_.explicitRoi)

      assertEquals(specphotoAfter, 1)
      assertEquals(twilightAfter, 1)
      assertEquals(twilightRoisAfter, List(GmosRoi.CentralSpectrum))
    }
  }

  test("Adding observation with different ROI creates new twilight calibration") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "Target1")
      tid2 <- createTargetAs(pi, pid, "Target2")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      // Set up first observation with FULL_FRAME
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid1" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: FULL_FRAME
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """,
      )
      _ <- prepareObservation(pi, oid1, tid1)
      // Generate calibrations for first observation
      _ <- recalculateCalibrations(pid, when)
      obsAfterFirst <- queryObservations(pid)
      // Now add second observation with same config but different ROI
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid2" } }
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                    explicitRoi: CENTRAL_SPECTRUM
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """,
      )
      _ <- prepareObservation(pi, oid2, tid2)
      // Recalculate calibrations after adding second observation
      _ <- recalculateCalibrations(pid, when)
      obsAfterSecond <- queryObservations(pid)
    } yield {
      // After first observation: 1 specphote + 1 Twilight
      val specphotoAfterFirst = obsAfterFirst.count(_.calibrationRole.exists(_ === CalibrationRole.SpectroPhotometric))
      val twilightAfterFirst = obsAfterFirst.count(_.calibrationRole.exists(_ === CalibrationRole.Twilight))

      assertEquals(specphotoAfterFirst, 1)
      assertEquals(twilightAfterFirst, 1)

      // After second observation: should have 1 specphote + 2 Twilight
      val specphotoAfterSecond = obsAfterSecond.count(_.calibrationRole.exists(_ === CalibrationRole.SpectroPhotometric))
      val twilightAfterSecond = obsAfterSecond.count(_.calibrationRole.exists(_ === CalibrationRole.Twilight))
      val twilightObs = obsAfterSecond.filter(_.calibrationRole.contains(CalibrationRole.Twilight))
      val rois = twilightObs.flatMap(_.observingMode).flatMap(_.gmosNorthLongSlit).flatMap(_.explicitRoi)

      // SpectroPhotometric calibrations ignore ROI, so still only 1
      assertEquals(specphotoAfterSecond, 1)
      // Twilight calibrations preserve ROI, so now 2 (one for each ROI)
      assertEquals(twilightAfterSecond, 2)
      assertEquals(rois.count(_ === GmosRoi.FullFrame), 1)
      assertEquals(rois.count(_ === GmosRoi.CentralSpectrum), 1)
    }
  }

  test("each calibration observation gets its own unique target") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      // two observations with different configs
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- updateConf(oid1, GmosAmpReadMode.Fast, GmosAmpGain.High)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid2)
      _    <- updateConf(oid2, GmosAmpReadMode.Slow, GmosAmpGain.Low)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      // run calibrations
      _    <- recalculateCalibrations(pid, when)
      ob   <- queryObservations(pid)
    } yield {
      val calibTargetIds = ob.collect:
        case CalibObs(_, _, Some(_), Some(CalibTE(Some(CalibTarget(targetId)))), _, _, _) =>
          targetId

      // Verify all calibration targets are unique
      assertEquals(calibTargetIds.size, calibTargetIds.distinct.size)
    }
  }

  test("events for deleting a calibration observation don't emit bogus notification") {
    for {
      pid  <- createProgram(pi, "foo")
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- recalculateCalibrations(pid, when)
      ev   <- Ref.of[IO, List[Notification[String]]](Nil)
      fib  <- withServices(service): services =>
                // listen to the raw notifications channel, ObsTopic filters bogus
                // event out
                services.session.channel(id"ch_observation_edit").listen(10)
                  .interruptAfter(10.seconds)
                  .compile.toList.flatTap(ev.set).start
      _    <- deleteObservation(pi, oid2)
      _    <- recalculateCalibrations(pid, when)
      _    <- fib.join
      _    <- ev.get.map: not =>
                // Bogus notifiactions carry no value
                val invalidNotifications = not.filter(_.value.isEmpty)

                // Without migration 1060 this produces a bogus notification without
                // a value, it only shwos on the logs butstill
                invalidNotifications.isEmpty
              .assert
    } yield ()
  }

  test("Don't generate calibrations when all GMOS science observations are not ready"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- setObservationWorkflowState(pi, oid1, ObservationWorkflowState.Inactive)
      _    <- setObservationWorkflowState(pi, oid2, ObservationWorkflowState.Inactive)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrations
      // No calibrations
      assertEquals(cCount, 0)
      assert(cgid.isEmpty)
    }

  test("Generate calibrations for active observation when mixed with inactive"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(pi, oid1, tid1) *> prepareObservation(pi, oid2, tid2)
      _    <- setObservationWorkflowState(pi, oid2, ObservationWorkflowState.Inactive)
      _    <- recalculateCalibrations(pid, when)
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val cgid = gr1.calibrationGroupId
      val cCount = ob.countCalibrations
      // 2 Calibrations for the only active obs
      assertEquals(cCount, 2)
      assert(cgid.isDefined)
    }

  test("Remove calibrations when science observation becomes inactive"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      _    <- recalculateCalibrations(pid, when)
      ob1  <- queryObservations(pid)
      gr1  <- groupElementsAs(pi, pid, None)
      // make the observation inactive
      _    <- setObservationWorkflowState(pi, oid1, ObservationWorkflowState.Inactive)
      _    <- recalculateCalibrations(pid, when)
      ob2  <- queryObservations(pid)
      gr2  <- groupElementsAs(pi, pid, None)
    } yield {
      val cCountBefore = ob1.countCalibrations
      val cgidBefore = gr1.calibrationGroupId
      val cCountAfter = ob2.countCalibrations
      val cgidAfter = gr2.calibrationGroupId
      // Before: 2 calibrations and group exists
      assertEquals(cCountBefore, 2)
      assert(cgidBefore.isDefined)
      // After: calibrations removed but group persists (empty)
      assertEquals(cCountAfter, 0)
      assert(cgidAfter.isDefined)
      assertEquals(cgidBefore, cgidAfter)
    }

  test("Remove calibrations when science observation is deleted"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(pi, oid1, tid1)
      _    <- recalculateCalibrations(pid, when)
      ob1  <- queryObservations(pid)
      gr1  <- groupElementsAs(pi, pid, None)
      // set existence to 'not present'
      _    <- deleteObservation(pi, oid1)
      _    <- recalculateCalibrations(pid, when)
      ob2  <- queryObservations(pid)
      gr2  <- groupElementsAs(pi, pid, None)
    } yield {
      val cCountBefore = ob1.countCalibrations
      val cgidBefore = gr1.calibrationGroupId
      val cCountAfter = ob2.countCalibrations
      val cgidAfter = gr2.calibrationGroupId
      assertEquals(cCountBefore, 2)
      assert(cgidBefore.isDefined)
      // calibrations removed when science observation is deleted
      assertEquals(cCountAfter, 0)
      assert(cgidAfter.isDefined)
    }

}
