// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import io.circe.refined.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.service.CalibrationsService

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

class calibrations extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val service  = TestUsers.service(3)

  val validUsers = List(pi, service)

  val when = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  case class CalibTarget(id: Target.Id) derives Decoder
  case class CalibTE(firstScienceTarget: Option[CalibTarget]) derives Eq, Decoder
  case class CalibObs(id: Observation.Id, groupId: Option[Group.Id], calibrationRole: Option[CalibrationRole], targetEnvironment: Option[CalibTE]) derives Decoder

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

  private def updateTargetProperties(tid: Target.Id, ra: Long, dec: Long, rv:  Double): IO[Json] =
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

  def unsetSED(tid: Target.Id): IO[Json] =
    query(
      pi,
      s"""
          mutation {
            updateTargets(input: {
              SET: {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: null
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

  def formatLD(ld: LocalDate): String = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")
    ld.atStartOfDay().atOffset(ZoneOffset.UTC).format(formatter)
  }

  private def scienceRequirements(oid: Observation.Id): IO[Json] =
    query(
      pi,
      s"""
          mutation {
            updateObservations(input: {
              SET: {
                scienceRequirements: {
                  mode: SPECTROSCOPY,
                  spectroscopy: {
                    wavelength: {
                      nanometers: 400.000
                    },
                    resolution: 10,
                    signalToNoise: 75.000,
                    signalToNoiseAt: {
                      nanometers: 410.000
                    },
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

  def prepareObservation(oid: Observation.Id, tid: Target.Id): IO[Unit] =
    for {
      _ <- updateTargetProperties(
             tid,
             RightAscension.Zero.toAngle.toMicroarcseconds,
             Declination.Zero.toAngle.toMicroarcseconds,
             0.0
           )
      _ <- scienceRequirements(oid)
    } yield ()

  test("no calibrations group if not needed") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, None)
      _   <- withServices(service) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
               }
             }
      gr1  <- groupElementsAs(pi, pid, None)
    } yield {
      val cgid = gr1.collect {
                case Left(gid) => gid
              }.headOption
      assert(cgid.isEmpty)
    }
  }

  test("create group for calibrations") {
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "One")
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      _   <- prepareObservation(oid, tid)
      gr  <- groupElementsAs(pi, pid, None)
      _   <- withServices(service) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
               }
             }
      gr1  <- groupElementsAs(pi, pid, None)
      cgid = gr1.collect {
                case Left(gid) => gid
              }.headOption
      cg   <- cgid.map(queryGroup)
                .getOrElse(IO.raiseError(new RuntimeException("No calibration group")))
      ob   <- queryObservations(pid)
    } yield {
      assertEquals(gr.size, 1)
      assert(cg._2)
      assertEquals(cg._3, CalibrationsService.CalibrationsGroupName)
    }
  }

  test("add calibrations for each LongSlit mode") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(oid1, tid1) *> prepareObservation(oid2, tid2)
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.collect { case Right(oid) => oid }
      val cgid = gr1.collect { case Left(gid) => gid }.headOption
      val cCount = ob.count {
        case CalibObs(_, _, Some(_), _) => true
        case _                       => false
      }
      // calibs belong to the calib group
      val obsGids = ob.collect {
        case CalibObs(_, Some(gid), _, _) => gid
      }
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 2)
      assertEquals(oids.size, 2)
    }
  }

  test("add calibrations for each LongSlit mode ignoring ones without conf") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
              // No target for oid2 -> no conf
      _    <- prepareObservation(oid1, tid1) *> scienceRequirements(oid2)
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.collect { case Right(oid) => oid }
      val cgid = gr1.collect { case Left(gid) => gid }.headOption
      val cCount = ob.count {
        case CalibObs(_, _, Some(_), _) => true
        case _                       => false
      }
      // calibs belong to the calib group
      val obsGids = ob.collect {
        case CalibObs(_, Some(gid), _, _) => gid
      }
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 1)
      assertEquals(oids.size, 2)
    }
  }

  val spectroPhotometricTargets: List[(String, Long, Long, Double, String)] =
    List(
      ("BD+28  4211",
        1180065332865L,
        103910367627L,
        0.000,
        """{
          "sourceProfile": {
            "point": {
                "emissionLines": null,
                "bandNormalized": {
                    "sed": null,
                    "brightnesses": [
                        {
                            "band": "U",
                            "error": null,
                            "units": "VEGA_MAGNITUDE",
                            "value": "8.922"
                        },
                        {
                            "band": "B",
                            "error": "0.03",
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.25"
                        },
                        {
                            "band": "V",
                            "error": "0.05",
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.58"
                        },
                        {
                            "band": "R",
                            "error": null,
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.656"
                        },
                        {
                            "band": "I",
                            "error": null,
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.831"
                        },
                        {
                            "band": "J",
                            "error": "0.026",
                            "units": "VEGA_MAGNITUDE",
                            "value": "11.275"
                        },
                        {
                            "band": "H",
                            "error": "0.036",
                            "units": "VEGA_MAGNITUDE",
                            "value": "11.438"
                        },
                        {
                            "band": "K",
                            "error": "0.028",
                            "units": "VEGA_MAGNITUDE",
                            "value": "11.556"
                        },
                        {
                            "band": "GAIA",
                            "error": "0.002851",
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.45304"
                        }
                    ]
                }
            },
            "uniform": null,
            "gaussian": null
          }
      }"""),
      ("BD+25  2534",
        681652742505L,
        90239867922L,
        0.000,
        """{
          "sourceProfile": {
            "point": {
                "emissionLines": null,
                "bandNormalized": {
                    "sed": null,
                    "brightnesses": [
                        {
                            "band": "U",
                            "error": null,
                            "units": "VEGA_MAGNITUDE",
                            "value": "8.922"
                        },
                        {
                            "band": "B",
                            "error": "0.03",
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.25"
                        },
                        {
                            "band": "V",
                            "error": "0.05",
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.58"
                        },
                        {
                            "band": "R",
                            "error": null,
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.656"
                        },
                        {
                            "band": "I",
                            "error": null,
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.831"
                        },
                        {
                            "band": "J",
                            "error": "0.026",
                            "units": "VEGA_MAGNITUDE",
                            "value": "11.275"
                        },
                        {
                            "band": "H",
                            "error": "0.036",
                            "units": "VEGA_MAGNITUDE",
                            "value": "11.438"
                        },
                        {
                            "band": "K",
                            "error": "0.028",
                            "units": "VEGA_MAGNITUDE",
                            "value": "11.556"
                        },
                        {
                            "band": "GAIA",
                            "error": "0.002851",
                            "units": "VEGA_MAGNITUDE",
                            "value": "10.45304"
                        }
                    ]
                }
            },
            "uniform": null,
            "gaussian": null
          }
      }""")
    )

  test("select calibration target") {
    for {
      tpid <- withServices(service) { s =>
                s.session.transaction.use { xa =>
                  s.programService
                    .insertCalibrationProgram(
                      ProgramPropertiesInput.Create(None).some,
                      CalibrationRole.SpectroPhotometric,
                      Description.unsafeFrom("SPECTROTEST"))(using xa)
                }
              }
      // Add some spectrophotometric targets
      _    <- spectroPhotometricTargets.traverse {  case (n, ra, dec, rv, s) =>
                 createTargetAs(service, tpid, n).flatTap(tid =>
                   updateTargetProperties(tid, ra, dec, rv)
                 )
              }
      // PI program
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      _    <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1).flatTap { oid =>
                prepareObservation(oid, tid1)
              }
      _    <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2).flatTap { oid =>
                prepareObservation(oid, tid2)
              }
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      ob   <- queryObservations(pid)
    } yield {
      val cCount = ob.count {
        case CalibObs(_, _, Some(_), Some(_)) => true
        case _                                => false
      }
      assertEquals(cCount, 2)
    }
  }

  test("add calibrations is idempotent") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(oid1, tid1) *> prepareObservation(oid2, tid2)
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa) *>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.collect { case Right(oid) => oid }
      val cgid = gr1.collect { case Left(gid) => gid }.headOption
      val cCount = ob.count {
        case CalibObs(_, _, Some(_), _) => true
        case _                          => false
      }
      // calibs belong to the calib group
      val obsGids = ob.collect {
        case CalibObs(_, Some(gid), _, _) => gid
      }
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 2)
      assertEquals(oids.size, 2)
    }
  }

  test("add calibrations is idempotent when an obs has no conf") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      tid2 <- createTargetAs(pi, pid, "Two")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      oid2 <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid2)
      _    <- prepareObservation(oid1, tid1) *> scienceRequirements(oid2)
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa) *>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      gr1  <- groupElementsAs(pi, pid, None)
      ob   <- queryObservations(pid)
    } yield {
      val oids = gr1.collect { case Right(oid) => oid }
      val cgid = gr1.collect { case Left(gid) => gid }.headOption
      val cCount = ob.count {
        case CalibObs(_, _, Some(_), _) => true
        case _                          => false
      }
      // calibs belong to the calib group
      val obsGids = ob.collect {
        case CalibObs(_, Some(gid), _, _) => gid
      }
      assert(obsGids.forall(g => cgid.exists(_ == g)))
      assertEquals(cCount, 1)
      assertEquals(oids.size, 2)
    }
  }

  test("calibration observations can't be modified by the pi") {
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "One")
      oid1 <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid1)
      _    <- prepareObservation(oid1, tid1)
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      ob   <- queryObservations(pid)
      cid = ob.collect {
        case CalibObs(cid, _, Some(_), _) => cid
      }
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
      _    <- prepareObservation(oid1, tid1)
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      ob   <- queryObservations(pid)
      cid = ob.collect {
        case CalibObs(cid, _, Some(_), _) => cid
      }
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
      _    <- prepareObservation(oid1, tid1)
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      ob   <- queryObservations(pid)
      (cid1, ct1) = ob.collect {
        case CalibObs(cid, _, Some(_), Some(ct1)) => (cid, ct1)
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
                   services.calibrationsService.recalculateCalibrationTarget(pid, cid1)(using xa)
                 }
               }
      ob2   <- queryObservations(pid)
      (cid2, ct2) = ob2.collect {
        case CalibObs(cid, _, Some(_), Some(ct2)) => (cid, ct2)
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
      _    <- prepareObservation(oid1, tid1)
      _    <- withServices(service) { services =>
                services.session.transaction.use { xa =>
                  services.calibrationsService.recalculateCalibrations(pid, when)(using xa)
                }
              }
      ob   <- queryObservations(pid)
      cid = ob.collect {
        case CalibObs(cid, _, Some(_), _) => cid
      }.head
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
                expected = List(s"Cannot clone calibration observations: $cid").asLeft
              )
    } yield ()
  }

}

