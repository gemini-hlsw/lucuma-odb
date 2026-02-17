// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.feature

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.clients.SimbadClientMock
import lucuma.catalog.clients.TelluricTargetsClientMock
import lucuma.catalog.telluric.TelluricStar
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.TelluricType
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.itc.client.SpectroscopyInput
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.graphql.subscription.SubscriptionUtils
import lucuma.odb.json.time.transport.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.odb.service.TelluricTargetsServiceSuiteSupport
import lucuma.refined.*
import org.http4s.client.UnexpectedStatus

import java.time.LocalDateTime
import java.time.ZoneOffset
import scala.collection.immutable.SortedMap

class perScienceObservationCalibrations
  extends OdbSuite
  with SubscriptionUtils
  with ExecutionTestSupportForFlamingos2
  with ObservingModeSetupOperations
  with TelluricTargetsServiceSuiteSupport:

  val DefaultSnAt: Wavelength = Wavelength.fromIntNanometers(510).get

  val when = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  // Mock telluric star
  val mockStarBefore = TelluricStar(
    hip = 12345,
    spType = TelluricType.A0V,
    coordinates = Coordinates(
      RightAscension.fromDoubleDegrees(123.456),
      Declination.fromDoubleDegrees(45.678).getOrElse(Declination.Zero)
    ),
    distance = 100.5,
    hmag = 7.5,
    score = 0.95,
    order = TelluricCalibrationOrder.Before
  )

  val mockStarAfter = TelluricStar(
    hip = 12346,
    spType = TelluricType.A0V,
    coordinates = Coordinates(
      RightAscension.fromDoubleDegrees(123.789),
      Declination.fromDoubleDegrees(45.123).getOrElse(Declination.Zero)
    ),
    distance = 98.2,
    hmag = 7.8,
    score = 0.92,
    order = TelluricCalibrationOrder.After
  )

  val mockTarget = Target.Sidereal(
    name = "HIP 12345".refined,
    tracking = SiderealTracking(
      baseCoordinates = mockStarBefore.coordinates,
      epoch = lucuma.core.math.Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    ),
    sourceProfile = SourceProfile.Point(
      SpectralDefinition.BandNormalized(None, SortedMap.empty)
    ),
    catalogInfo = None
  )

  val mockTelluricJson: Json = Json.obj(
    "data" -> Json.obj(
      "search" -> Json.arr(
        Json.obj(
          "HIP" -> mockStarBefore.hip.asJson,
          "spType" -> Json.fromString(mockStarBefore.spType.tag),
          "RA" -> mockStarBefore.coordinates.ra.toAngle.toDoubleDegrees.asJson,
          "Dec" -> mockStarBefore.coordinates.dec.toAngle.toSignedDoubleDegrees.asJson,
          "Distance" -> mockStarBefore.distance.asJson,
          "Hmag" -> mockStarBefore.hmag.asJson,
          "Score" -> mockStarBefore.score.asJson,
          "Order" -> Json.fromString(mockStarBefore.order.tag)
        ),
        Json.obj(
          "HIP" -> mockStarAfter.hip.asJson,
          "spType" -> Json.fromString(mockStarAfter.spType.tag),
          "RA" -> mockStarAfter.coordinates.ra.toAngle.toDoubleDegrees.asJson,
          "Dec" -> mockStarAfter.coordinates.dec.toAngle.toSignedDoubleDegrees.asJson,
          "Distance" -> mockStarAfter.distance.asJson,
          "Hmag" -> mockStarAfter.hmag.asJson,
          "Score" -> mockStarAfter.score.asJson,
          "Order" -> Json.fromString(mockStarAfter.order.tag)
        )
      )
    )
  )

  override protected def telluricClient: IO[TelluricTargetsClient[IO]] =
    TelluricTargetsClientMock.fromJson(mockTelluricJson, SimbadClientMock.withSingleTarget(mockTarget))

  // Override fake ITC to vary duration based on exposure time mode
  override def fakeItcSpectroscopyResultFor(input: SpectroscopyInput): Option[IntegrationTime] =
    input.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(time, count, _) =>
        Some(IntegrationTime(time, count))
      case ExposureTimeMode.SignalToNoiseMode(sn, _) =>
        Some(IntegrationTime(5.minuteTimeSpan, PosInt.unsafeFrom(4)))

  case class GroupInfo(
    id:               Group.Id,
    system:           Boolean,
    name:             String,
    ordered:          Boolean,
    minimumRequired:  Option[Int],
    maximumInterval:  Option[TimeSpan],
    calibrationRoles: List[CalibrationRole],
    parentId:         Option[Group.Id],
    parentIndex:      Int
  ) derives Decoder

  case class ObsInfo(
    id:              Observation.Id,
    groupId:         Option[Group.Id],
    groupIndex:      Option[Int],
    calibrationRole: Option[CalibrationRole]
  ) derives Decoder

  case class ObsWithTarget(
    id:         Observation.Id,
    targetId:   Option[lucuma.core.model.Target.Id],
    targetName: Option[String],
    targetRa:   Option[String],
    targetDec:  Option[String]
  ) derives Decoder

  case class ExposureTimeModeInfo(
    snValue:  Option[SignalToNoise],
    snWAt:    Option[Wavelength],
    txcValue: Option[BigDecimal],
    txcCount: Option[Int],
    txcWvAt:  Option[Wavelength]
  )

  case class AllEtmInfo(
    requirement: ExposureTimeModeInfo,
    acquisition: ExposureTimeModeInfo,
    science:     ExposureTimeModeInfo
  )

  private def queryExposureTimeMode(oid: Observation.Id): IO[ExposureTimeModeInfo] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              scienceRequirements {
                exposureTimeMode {
                  signalToNoise {
                    value
                    at { picometers }
                  }
                  timeAndCount {
                    time { minutes }
                    count
                    at { picometers }
                  }
                }
              }
            }
          }"""
    ).map { c =>
      val etm = c.hcursor
        .downField("observation")
        .downField("scienceRequirements")
        .downField("exposureTimeMode")
      val snValue = etm.downField("signalToNoise").downField("value").as[BigDecimal].toOption
        .flatMap(v => SignalToNoise.FromBigDecimalExact.getOption(v))
      val snWAt = etm.downField("signalToNoise").downField("at").as[Wavelength].toOption
      val txcValue = etm.downField("timeAndCount").downField("time").downField("minutes").as[BigDecimal].toOption
      val txcCount = etm.downField("timeAndCount").downField("count").as[Int].toOption
      val txcWvAt = etm.downField("timeAndCount").downField("at").as[Wavelength].toOption
      ExposureTimeModeInfo(snValue, snWAt, txcValue, txcCount, txcWvAt)
    }

  private def queryAllEtms(oid: Observation.Id): IO[AllEtmInfo] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              scienceRequirements {
                exposureTimeMode {
                  signalToNoise {
                    value
                    at { picometers }
                  }
                }
              }
              observingMode {
                flamingos2LongSlit {
                  acquisition {
                    exposureTimeMode {
                      signalToNoise {
                        value
                        at { picometers }
                      }
                    }
                  }
                  exposureTimeMode {
                    signalToNoise {
                      value
                      at { picometers }
                    }
                  }
                }
              }
            }
          }"""
    ).map { c =>
      val obs = c.hcursor.downField("observation")
      val reqEtm = obs.downField("scienceRequirements").downField("exposureTimeMode")
      val f2 = obs.downField("observingMode").downField("flamingos2LongSlit")
      val acqEtm = f2.downField("acquisition").downField("exposureTimeMode")
      val sciEtm = f2.downField("exposureTimeMode")

      def parseEtm(cursor: io.circe.ACursor): ExposureTimeModeInfo =
        ExposureTimeModeInfo(
          cursor.downField("signalToNoise").downField("value").as[BigDecimal].toOption
            .flatMap(v => SignalToNoise.FromBigDecimalExact.getOption(v)),
          cursor.downField("signalToNoise").downField("at").as[Wavelength].toOption,
          None, None, None
        )

      AllEtmInfo(parseEtm(reqEtm), parseEtm(acqEtm), parseEtm(sciEtm))
    }

  private def queryObservationsInGroup(gid: Group.Id): IO[List[ObsInfo]] =
    query(
      serviceUser,
      s"""query {
            group(groupId: "$gid") {
              elements {
                observation {
                  id
                  groupId
                  groupIndex
                  calibrationRole
                }
              }
            }
          }"""
    ).flatMap { c =>
      val elements = c.hcursor
        .downField("group")
        .downField("elements")
        .values
        .toList
        .flatten
        .flatMap(_.hcursor.downField("observation").as[Option[ObsInfo]].toOption)
        .flatten
      elements.pure[IO]
    }

  private def queryObservationWithTarget(oid: Observation.Id): IO[ObsWithTarget] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              id
              targetEnvironment {
                asterism {
                  id
                  name
                  sidereal {
                    ra { hms }
                    dec { dms }
                  }
                }
              }
            }
          }"""
    ).flatMap { c =>
      val cursor = c.hcursor.downField("observation")
      val asterismCursor = cursor
        .downField("targetEnvironment")
        .downField("asterism")
        .downArray

      val result = for
        id <- cursor.downField("id").as[Observation.Id]
        targetId <- asterismCursor.downField("id").as[Option[lucuma.core.model.Target.Id]]
        targetName <- asterismCursor.downField("name").as[Option[String]]
        targetRa <- asterismCursor.downField("sidereal").downField("ra").downField("hms").as[Option[String]]
        targetDec <- asterismCursor.downField("sidereal").downField("dec").downField("dms").as[Option[String]]
      yield ObsWithTarget(id, targetId, targetName, targetRa, targetDec)

      result.leftMap(f => new RuntimeException(f.message)).liftTo[IO]
    }

  private def queryObservationExists(oid: Observation.Id): IO[Boolean] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              id
            }
          }"""
    ).map { c =>
      c.hcursor.downField("observation").as[ObsInfo].isRight
    }

  private def queryTargetExists(tid: lucuma.core.model.Target.Id): IO[Boolean] =
    query(
      serviceUser,
      s"""query {
            target(targetId: "$tid") {
              id
            }
          }"""
    ).map { c =>
      c.hcursor.downField("target").focus.exists(!_.isNull)
    }

  private def queryTargetSed(tid: lucuma.core.model.Target.Id): IO[Option[String]] =
    query(
      serviceUser,
      s"""query {
            target(targetId: "$tid") {
              sourceProfile {
                point {
                  bandNormalized {
                    sed { stellarLibrary }
                  }
                }
              }
            }
          }"""
    ).map { c =>
      c.hcursor
        .downField("target")
        .downField("sourceProfile")
        .downField("point")
        .downField("bandNormalized")
        .downField("sed")
        .downField("stellarLibrary")
        .as[String]
        .toOption
    }

  private def queryGroup(gid: Group.Id): IO[GroupInfo] =
    query(
      serviceUser,
      s"""query {
            group(groupId: "$gid") {
              id
              system
              name
              ordered
              calibrationRoles
              minimumRequired
              maximumInterval {
                microseconds
              }
              parentId
              parentIndex
            }
          }"""
    ).flatMap { c =>
      c.hcursor.downField("group").as[GroupInfo]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  private def queryObservation(oid: Observation.Id): IO[ObsInfo] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              id
              groupId
              groupIndex
              calibrationRole
            }
          }"""
    ).flatMap { c =>
      c.hcursor.downField("observation").as[ObsInfo]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  private def queryGroupExists(gid: Group.Id): IO[Boolean] =
    query(
      serviceUser,
      s"""query {
            group(groupId: "$gid") {
              id
            }
          }"""
    ).map { c =>
      val groupJson = c.hcursor.downField("group").focus
      groupJson.exists(!_.isNull)
    }

  private def updateFlamingos2Fpu(oid: Observation.Id, fpu: Flamingos2Fpu): IO[Unit] =
    val fpuTag = fpu match
      case Flamingos2Fpu.LongSlit1 => "LONG_SLIT_1"
      case Flamingos2Fpu.LongSlit2 => "LONG_SLIT_2"
      case Flamingos2Fpu.LongSlit3 => "LONG_SLIT_3"
      case Flamingos2Fpu.LongSlit4 => "LONG_SLIT_4"
      case Flamingos2Fpu.LongSlit6 => "LONG_SLIT_6"
      case Flamingos2Fpu.LongSlit8 => "LONG_SLIT_8"
      case _                       => fpu.tag.toUpperCase.replace("LONGSLIT", "LONG_SLIT")
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: {
            observingMode: {
              flamingos2LongSlit: {
                fpu: $fpuTag
              }
            }
          }
        }) {
          observations {
            id
          }
        }
      }"""
    ).void

  private def setScienceRequirements(oid: Observation.Id, snWAt: Wavelength = DefaultSnAt, snValue: Double = 75.0): IO[Unit] =
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          SET: {
            scienceRequirements: {
              exposureTimeMode: {
                signalToNoise: {
                  value: $snValue,
                  at: { nanometers: ${snWAt.toNanometers} }
                }
              },
              spectroscopy: {
                wavelength: { nanometers: 1390.000 },
                resolution: 10,
                wavelengthCoverage: { nanometers: 0.010 },
                focalPlane: SINGLE_SLIT,
                focalPlaneAngle: { arcseconds: 5 }
              }
            },
            observingMode: {
              flamingos2LongSlit: {
                exposureTimeMode: {
                  signalToNoise: {
                    value: $snValue,
                    at: { nanometers: ${snWAt.toNanometers} }
                  }
                }
              }
            }
          }
          WHERE: { id: { EQ: "$oid" } }
        }) {
          observations { id }
        }
      }"""
    ).void

  private def setScienceRequirementsTimeAndCount(
    oid: Observation.Id,
    atNm: BigDecimal = BigDecimal(1390),
    timeMinutes: Int = 10,
    count: Int = 6
  ): IO[Unit] =
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          SET: {
            scienceRequirements: {
              exposureTimeMode: {
                timeAndCount: {
                  time: { minutes: $timeMinutes },
                  count: $count,
                  at: { nanometers: $atNm }
                }
              },
              spectroscopy: {
                wavelength: { nanometers: 1390.000 },
                resolution: 10,
                wavelengthCoverage: { nanometers: 0.010 },
                focalPlane: SINGLE_SLIT,
                focalPlaneAngle: { arcseconds: 5 }
              }
            },
            observingMode: {
              flamingos2LongSlit: {
                exposureTimeMode: {
                  timeAndCount: {
                    time: { minutes: $timeMinutes },
                    count: $count,
                    at: { nanometers: $atNm }
                  }
                }
              }
            }
          }
          WHERE: { id: { EQ: "$oid" } }
        }) {
          observations { id }
        }
      }"""
    ).void

  private def queryObservationFpu(oid: Observation.Id): IO[Option[Flamingos2Fpu]] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              id
              observingMode {
                flamingos2LongSlit {
                  fpu
                }
              }
            }
          }"""
    ).map: c =>
      c.hcursor
        .downField("observation")
        .downField("observingMode")
        .downField("flamingos2LongSlit")
        .downField("fpu")
        .as[Flamingos2Fpu]
        .toOption

  private def queryAllGroups(pid: Program.Id): IO[List[Group.Id]] =
    case class GroupWrapper(id: Group.Id) derives Decoder
    case class GroupElementWrapper(group: Option[GroupWrapper]) derives Decoder
    query(
      serviceUser,
      s"""query {
            program(programId: "$pid") {
              allGroupElements {
                group {
                  id
                }
              }
            }
          }"""
    ).flatMap { c =>
      c.hcursor
        .downField("program")
        .downField("allGroupElements")
        .as[List[GroupElementWrapper]]
        .map(_.flatMap(_.group.map(_.id)))
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  private def updateObservationMode(oid: Observation.Id, mode: String): IO[Unit] =
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: {
            observingMode: {
              $mode: {
                grating: R831_G5302
                filter: R_PRIME
                fpu: LONG_SLIT_0_50
                centralWavelength: {
                  nanometers: 500
                }
                explicitYBin: TWO
              }
            }
          }
        }) {
          observations {
            id
          }
        }
      }"""
    ).void

  private def setObservationInactive(oid: Observation.Id): IO[Unit] =
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: {
            existence: DELETED
          }
        }) {
          observations {
            id
          }
        }
      }"""
    ).void

  test("F2 observation is automatically placed in a group with telluric role"):
    for {
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _    <- runObscalcUpdate(pid, oid)
      _    <- recalculateCalibrations(pid, when)
      obs  <- queryObservation(oid)
      grp  <- obs.groupId.traverse(queryGroup)
    } yield {
      assert(obs.groupId.isDefined)
      assert(grp.exists(_.system))
      assert(grp.exists(_.calibrationRoles.contains(CalibrationRole.Telluric)))
    }

  test("each F2 observations gets its own group"):
    for {
      pid   <- createProgramAs(pi)
      tid1  <- createTargetWithProfileAs(pi, pid)
      tid2  <- createTargetWithProfileAs(pi, pid)
      oid1  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      _     <- runObscalcUpdate(pid, oid1)
      oid2  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      _     <- runObscalcUpdate(pid, oid2)
      _     <- recalculateCalibrations(pid, when)
      obs1  <- queryObservation(oid1)
      obs2  <- queryObservation(oid2)
    } yield {
      assert(obs1.groupId.isDefined)
      assert(obs2.groupId.isDefined)
      assert(obs1.groupId =!= obs2.groupId)
    }

  test("Changing F2 to GMOS removes the group"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetWithProfileAs(pi, pid)
      oid         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _           <- runObscalcUpdate(pid, oid)
      _           <- recalculateCalibrations(pid, when)
      obsBefore   <- queryObservation(oid)
      groupId     =  obsBefore.groupId.get
      // Change observation to GMOS
      _           <- updateObservationMode(oid, "gmosNorthLongSlit")
      _           <- recalculateCalibrations(pid, when)
      obsAfter    <- queryObservation(oid)
      // Group should be deleted
      groupExists <- queryGroupExists(groupId)
    } yield {
      assert(obsBefore.groupId.isDefined)
      assert(obsAfter.groupId.isEmpty)
      assert(!groupExists)
    }

  test("Deleting F2 removes group"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetWithProfileAs(pi, pid)
      oid         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _           <- runObscalcUpdate(pid, oid)
      _           <- recalculateCalibrations(pid, when)
      obsBefore   <- queryObservation(oid)
      groupId     =  obsBefore.groupId.get
      // Set observation to inactive (deleted)
      _           <- setObservationInactive(oid)
      _           <- recalculateCalibrations(pid, when)
      // Group should be deleted
      groupExists <- queryGroupExists(groupId)
    } yield {
      assert(obsBefore.groupId.isDefined)
      assert(!groupExists)
    }

  test("telluric group has immediate execution properties"):
    for {
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      oid       <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _         <- runObscalcUpdate(pid, oid)
      _         <- recalculateCalibrations(pid, when)
      obs       <- queryObservation(oid)
      groupId   =  obs.groupId.get
      groupInfo <- queryGroup(groupId)
    } yield {
      assertEquals(groupInfo.ordered, true)
      assertEquals(groupInfo.minimumRequired, None) // all observations
      assertEquals(groupInfo.maximumInterval, TimeSpan.Zero.some)
    }

  test("Multiple recalculations are idempotent, no duplicate groups"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetWithProfileAs(pi, pid)
      oid         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _           <- runObscalcUpdate(pid, oid)
      _           <- recalculateCalibrations(pid, when)
      obsAfter1   <- queryObservation(oid)
      groupId1    =  obsAfter1.groupId.get
      allGroups1  <- queryAllGroups(pid)
      _           <- recalculateCalibrations(pid, when)
      obsAfter2   <- queryObservation(oid)
      groupId2    =  obsAfter2.groupId.get
      allGroups2  <- queryAllGroups(pid)
      _           <- recalculateCalibrations(pid, when)
      obsAfter3   <- queryObservation(oid)
      groupId3    =  obsAfter3.groupId.get
      allGroups3  <- queryAllGroups(pid)
    } yield {
      assertEquals(groupId1, groupId2)
      assertEquals(groupId2, groupId3)
      assertEquals(allGroups3.length, 1)
      assertEquals(allGroups1.toSet, allGroups2.toSet)
      assertEquals(allGroups2.toSet, allGroups3.toSet)
    }

  test("Mixed add/delete in single recalculation"):
    for {
      pid         <- createProgramAs(pi)
      tid1        <- createTargetWithProfileAs(pi, pid)
      tid2        <- createTargetWithProfileAs(pi, pid)
      tid3        <- createTargetWithProfileAs(pi, pid)
      oid1        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      _           <- runObscalcUpdate(pid, oid1)
      oid2        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      _           <- runObscalcUpdate(pid, oid2)
      _           <- recalculateCalibrations(pid, when)
      obs1Before  <- queryObservation(oid1)
      obs2Before  <- queryObservation(oid2)
      group1Id    =  obs1Before.groupId.get
      // Delete obs1, keep obs2, add obs3
      _           <- setObservationInactive(oid1)
      oid3        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid3))
      _           <- runObscalcUpdate(pid, oid3)
      _           <- recalculateCalibrations(pid, when)
      group1Exists <- queryGroupExists(group1Id)
      obs2After   <- queryObservation(oid2)
      obs3After   <- queryObservation(oid3)
    } yield {
      assert(!group1Exists)
      assert(obs2After.groupId.isDefined)
      assert(obs3After.groupId.isDefined)
    }

  test("New F2 observation creates group even with inactive F2 observation present"):
    for {
      pid         <- createProgramAs(pi)
      tid1        <- createTargetWithProfileAs(pi, pid)
      tid2        <- createTargetWithProfileAs(pi, pid)
      oid1        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      _           <- runObscalcUpdate(pid, oid1)
      _           <- recalculateCalibrations(pid, when)
      obs1Before  <- queryObservation(oid1)
      group1Id    =  obs1Before.groupId.get
      // Set observation1 inactive
      _           <- setObservationInactive(oid1)
      // Create new F2 observation
      oid2        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      _           <- runObscalcUpdate(pid, oid2)
      _           <- recalculateCalibrations(pid, when)
      group1Exists <- queryGroupExists(group1Id)
      obs2After   <- queryObservation(oid2)
    } yield {
      assert(!group1Exists)
      assert(obs1Before.groupId =!= obs2After.groupId)
      assert(obs2After.groupId != Some(group1Id))
    }

  test("Recalculation handles GMOS observation correctly"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetWithProfileAs(pi, pid)
      oid         <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _           <- recalculateCalibrations(pid, when)
      obs         <- queryObservation(oid)
    } yield {
      assertEquals(obs.groupId, None)
    }

  test("Observation takes telluric group's position when group is deleted"):
    for {
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _    <- runObscalcUpdate(pid, oid)
      _    <- recalculateCalibrations(pid, when)
      obs  <- queryObservation(oid)
      telluricGroupId = obs.groupId.get
      telluricGroup <- queryGroup(telluricGroupId)
      // Change to GMOS
      _    <- updateObservationMode(oid, "gmosNorthLongSlit")
      _    <- recalculateCalibrations(pid, when)
      obsAfter <- queryObservation(oid)
    } yield {
      assertEquals(obsAfter.groupId, telluricGroup.parentId)
      assertEquals(obsAfter.groupIndex, Some(telluricGroup.parentIndex))
    }

  test("Observation preserves parent group position when nested telluric group is deleted"):
    for {
      pid              <- createProgramAs(pi)
      parentGroupId    <- createGroupAs(pi, pid, name = "parent".some)
      tid              <- createTargetWithProfileAs(pi, pid)
      // Create F2 observation directly in the parent group
      oid              <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                <- moveObservationAs(pi, oid, parentGroupId.some)  // Move to parent group
      _                <- runObscalcUpdate(pid, oid)
      _                <- recalculateCalibrations(pid, when)
      obs              <- queryObservation(oid)
      telluricGroupId  =  obs.groupId.get
      telluricGroup    <- queryGroup(telluricGroupId)
      _                <- updateObservationMode(oid, "gmosNorthLongSlit")
      _                <- recalculateCalibrations(pid, when)
      obsAfter         <- queryObservation(oid)
      groupAfter       <- queryGroupExists(telluricGroupId)
    } yield {
      assertEquals(telluricGroup.parentId, Some(parentGroupId))
      assert(telluricGroup.system)
      assert(telluricGroup.calibrationRoles.contains(CalibrationRole.Telluric))
      // Verify observation moved back to parent group at telluric group's position
      assertEquals(obsAfter.groupId, Some(parentGroupId))
      assertEquals(obsAfter.groupIndex, Some(telluricGroup.parentIndex))
      assert(!groupAfter)
    }

  test("Telluric group uses obs position in the group"):
    for {
      pid              <- createProgramAs(pi)
      parentGroupId    <- createGroupAs(pi, pid, name = "parent".some)
      tid1             <- createTargetWithProfileAs(pi, pid)
      tid2             <- createTargetWithProfileAs(pi, pid)
      f2Oid            <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      _                <- moveObservationAs(pi, f2Oid, parentGroupId.some)
      // Add gmos after f2
      gmosOid          <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid2))
      _                <- moveObservationAs(pi, gmosOid, parentGroupId.some)
      f2Before         <- queryObservation(f2Oid)
      gmosBefore       <- queryObservation(gmosOid)
      originalGroupId  =  f2Before.groupId
      originalIndex    =  f2Before.groupIndex
      _                <- runObscalcUpdate(pid, f2Oid)
      _                <- recalculateCalibrations(pid, when)
      // check it is idempotent
      _                <- recalculateCalibrations(pid, when)
      f2After          <- queryObservation(f2Oid)
      gmosAfter        <- queryObservation(gmosOid)
      telluricGroupId  =  f2After.groupId.get
      telluricGroup    <- queryGroup(telluricGroupId)
    } yield {
      // F2 was originally at index 0, GMOS at index 1
      assertEquals(f2Before.groupIndex, 0.some)
      assertEquals(gmosBefore.groupIndex, 1.some)
      // Verify telluric group took the F2 observation's original position
      assertEquals(telluricGroup.parentId, originalGroupId)
      assertEquals(telluricGroup.parentIndex.some, originalIndex)
      // GMOS observation stays at index 1
      assertEquals(gmosAfter.groupId, parentGroupId.some)
      assertEquals(gmosAfter.groupIndex, 1.some)
    }

  test("F2 observation gets a telluric calibration observation"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- runObscalcUpdate(pid, oid)
      (added, removed)   <- recalculateCalibrations(pid, when)
      obs                <- queryObservation(oid)
      groupId            =  obs.groupId.get
      obsInGroup         <- queryObservationsInGroup(groupId)
      telluricObs        =  obsInGroup.filter(_.calibrationRole.contains(CalibrationRole.Telluric))
    } yield {
      assertEquals(telluricObs.size, 1)
      assertEquals(obsInGroup.size, 2)
      assertEquals(added.size, 1)
      assertEquals(added.headOption, telluricObs.headOption.map(_.id))
      assertEquals(removed.size, 0)
    }

  test("telluric observation can get a real target"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetWithProfileAs(pi, pid)
      oid         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _           <- setScienceRequirements(oid)
      _           <- runObscalcUpdate(pid, oid)
      _           <- recalculateCalibrations(pid, when)
      _           <- sleep >> resolveTelluricTargets
      obs         <- queryObservation(oid)
      groupId     =  obs.groupId.get
      obsInGroup  <- queryObservationsInGroup(groupId)
      telluricOid =  obsInGroup.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricObs <- queryObservationWithTarget(telluricOid)
    } yield {
      assertEquals(telluricObs.targetName, Some("HIP 12345"))
      assertEquals(telluricObs.targetRa, Some("08:13:49.440000"))
      assertEquals(telluricObs.targetDec, Some("+45:40:40.800000"))
    }

  test("telluric observation targets are not shared"):
    for {
      pid                <- createProgramAs(pi)
      tid1               <- createTargetWithProfileAs(pi, pid)
      tid2               <- createTargetWithProfileAs(pi, pid)
      oid1               <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      _                  <- runObscalcUpdate(pid, oid1)
      oid2               <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      _                  <- runObscalcUpdate(pid, oid2)
      (added, removed)   <- recalculateCalibrations(pid, when)
      _                  <- sleep >> resolveTelluricTargets
      obs1               <- queryObservation(oid1)
      obs2               <- queryObservation(oid2)
      obsInGroup1        <- queryObservationsInGroup(obs1.groupId.get)
      obsInGroup2        <- queryObservationsInGroup(obs2.groupId.get)
      telluric1Oid       =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluric2Oid       =  obsInGroup2.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluric1          <- queryObservationWithTarget(telluric1Oid)
      telluric2          <- queryObservationWithTarget(telluric2Oid)
    } yield {
      assertNotEquals(telluric1Oid, telluric2Oid)
      assertNotEquals(telluric1.targetId, telluric2.targetId)
      assertEquals(added.size, 2)
      assert(added.contains(telluric1Oid))
      assert(added.contains(telluric2Oid))
      assertEquals(removed.size, 0)
    }

  test("calling recalculateCalibrations multiple times is idempotent"):
    for {
      pid                  <- createProgramAs(pi)
      tid                  <- createTargetWithProfileAs(pi, pid)
      oid                  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                    <- runObscalcUpdate(pid, oid)
      (added1, removed1)   <- recalculateCalibrations(pid, when)
      obs                  <- queryObservation(oid)
      groupId              =  obs.groupId.get
      obsInGroup1          <- queryObservationsInGroup(groupId)
      telluric1Oid         =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      (added2, removed2)   <- recalculateCalibrations(pid, when)
      obsInGroup2          <- queryObservationsInGroup(groupId)
      telluric2Oid         =  obsInGroup2.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
    } yield {
      assertEquals(obsInGroup2.size, 2)
      assertEquals(telluric1Oid, telluric2Oid)
      assertEquals(added1.size, 1)
      assertEquals(removed1.size, 0)
      assertEquals(added2.size, 0)
      assertEquals(removed2.size, 0)
    }

  test("recalculation syncs telluric observation configuration"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      obs1         <- queryObservation(oid)
      groupId      =  obs1.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluric1Oid =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      scienceConds <- queryObservationConstraints(oid)
      conds1       <- queryObservationConstraints(telluric1Oid)
      _            <- recalculateCalibrations(pid, when)
      obsInGroup2  <- queryObservationsInGroup(groupId)
      telluric2Oid =  obsInGroup2.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      conds2       <- queryObservationConstraints(telluric2Oid)
    } yield {
      assertEquals(obsInGroup2.size, 2)
      assertEquals(telluric1Oid, telluric2Oid)
      // Verify conditions
      assertEquals(conds1, scienceConds)
      assertEquals(conds2, scienceConds)
    }

  test("changing F2 to GMOS deletes telluric observation"):
    for {
      pid                  <- createProgramAs(pi)
      tid                  <- createTargetWithProfileAs(pi, pid)
      oid                  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                    <- runObscalcUpdate(pid, oid)
      (added1, removed1)   <- recalculateCalibrations(pid, when)
      obs1                 <- queryObservation(oid)
      groupId              =  obs1.groupId.get
      obsInGroup1          <- queryObservationsInGroup(groupId)
      telluricOid          =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      _                    <- updateObservationMode(oid, "gmosNorthLongSlit")
      (added2, removed2)   <- recalculateCalibrations(pid, when)
      obsExists            <- queryObservationExists(telluricOid)
      groupExists          <- queryGroupExists(groupId)
    } yield {
      assert(!obsExists)
      assert(!groupExists)
      assertEquals(added1.size, 1)
      assertEquals(removed1.size, 0)
      assertEquals(removed2.size, 1)
      assertEquals(removed2.headOption, telluricOid.some)
    }

  test("deleting F2 science observation deletes telluric observation"):
    for {
      pid                  <- createProgramAs(pi)
      tid                  <- createTargetWithProfileAs(pi, pid)
      oid                  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                    <- runObscalcUpdate(pid, oid)
      (added1, removed1)   <- recalculateCalibrations(pid, when)
      _                    <- sleep >> resolveTelluricTargets
      obs1                 <- queryObservation(oid)
      groupId              =  obs1.groupId.get
      obsInGroup1          <- queryObservationsInGroup(groupId)
      telluricOid          =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricTgt          <- queryObservationWithTarget(telluricOid)
      telluricTid          =  telluricTgt.targetId.get
      _                    <- setObservationInactive(oid)
      (added2, removed2)   <- recalculateCalibrations(pid, when)
      tellExists           <- queryObservationExists(telluricOid)
      groupExists          <- queryGroupExists(groupId)
      targetExists         <- queryTargetExists(telluricTid)
    } yield {
      assert(!tellExists)
      assert(!groupExists)
      assert(!targetExists)
      assertEquals(added1.size, 1)
      assertEquals(removed1.size, 0)
      assertEquals(added2.size, 0)
      assertEquals(removed2.size, 1)
      assertEquals(removed2.headOption, telluricOid.some)
    }

  test("recalculateCalibrations returns correct added and removed observation IDs"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- runObscalcUpdate(pid, oid)
      // add 1 telluric
      (added1, removed1) <- recalculateCalibrations(pid, when)
      obs1               <- queryObservation(oid)
      groupId            =  obs1.groupId.get
      obsInGroup1        <- queryObservationsInGroup(groupId)
      telluricOid        =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      // re-sync the same observation (idempotent)
      (added2, removed2) <- recalculateCalibrations(pid, when)
      // removed F2 observation should remove the telluric
      _                  <- setObservationInactive(oid)
      (added3, removed3) <- recalculateCalibrations(pid, when)
    } yield {
      // First recalculation added the telluric observation
      assertEquals(added1.size, 1)
      assertEquals(added1.headOption, telluricOid.some)
      assertEquals(removed1.size, 0)
      // Second recalculation is truly idempotent (re-syncs existing, returns nothing new)
      assertEquals(added2.size, 0)
      assertEquals(removed2.size, 0)
      // Third recalculation removed the telluric observation after deleting science obs
      assertEquals(added3.size, 0)
      assertEquals(removed3.size, 1)
      assertEquals(removed3.headOption, telluricOid.some)
    }

  test("changing F2 FPU syncs to telluric observation"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      obs1         <- queryObservation(oid)
      groupId      =  obs1.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      scienceFpu1  <- queryObservationFpu(oid)
      telluricFpu1 <- queryObservationFpu(telluricOid)
      _            <- updateFlamingos2Fpu(oid, Flamingos2Fpu.LongSlit2)
      _            <- runObscalcUpdate(pid, oid)
      _            <- sleep >> recalculateCalibrations(pid, when)
      scienceFpu2  <- queryObservationFpu(oid)
      telluricFpu2 <- queryObservationFpu(telluricOid)
    } yield {
      assertEquals(scienceFpu1, Flamingos2Fpu.LongSlit1.some)
      assertEquals(telluricFpu1, Flamingos2Fpu.LongSlit1.some)
      assertEquals(scienceFpu2, Flamingos2Fpu.LongSlit2.some)
      assertEquals(telluricFpu2, Flamingos2Fpu.LongSlit2.some)
    }

  test("Don't generate for inactive"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetWithProfileAs(pi, pid)
      tid2 <- createTargetWithProfileAs(pi, pid)
      oid1 <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      oid2 <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      // Explicitly set both observations to Inactive workflow state
      _    <- setObservationWorkflowState(pi, oid1, ObservationWorkflowState.Inactive)
      _    <- setObservationWorkflowState(pi, oid2, ObservationWorkflowState.Inactive)
      _    <- recalculateCalibrations(pid, when)
      obs1 <- queryObservation(oid1)
      obs2 <- queryObservation(oid2)
    } yield {
      // No telluric groups created
      assertEquals(obs1.groupId, None)
      assertEquals(obs2.groupId, None)
    }

  test("Generate for mixed observation ready and inactive"):
    for {
      pid         <- createProgramAs(pi)
      tid1        <- createTargetWithProfileAs(pi, pid)
      tid2        <- createTargetWithProfileAs(pi, pid)
      oid1        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      _           <- runObscalcUpdate(pid, oid1)
      oid2        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      _           <- setObservationWorkflowState(pi, oid2, ObservationWorkflowState.Inactive)
      _           <- recalculateCalibrations(pid, when)
      obs1        <- queryObservation(oid1)
      obs2        <- queryObservation(oid2)
      obsInGroup1 <- obs1.groupId.traverse(queryObservationsInGroup)
    } yield {
      // oid1 should have telluric group
      assert(obs1.groupId.isDefined)
      assertEquals(obsInGroup1.map(_.size), Some(2))
      // oid2 should not have telluric group
      assertEquals(obs2.groupId, None)
    }

  test("Remove telluric when observation becomes inactive or deleted"):
    List(
      (oid: Observation.Id) => setObservationWorkflowState(pi, oid, ObservationWorkflowState.Inactive),
      (oid: Observation.Id) => setObservationInactive(oid)
    ).traverse_ : set =>
      for {
        pid          <- createProgramAs(pi)
        tid          <- createTargetWithProfileAs(pi, pid)
        oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
        _            <- runObscalcUpdate(pid, oid)
        _            <- recalculateCalibrations(pid, when)
        _            <- sleep >> resolveTelluricTargets
        obsBefore    <- queryObservation(oid)
        groupId      =  obsBefore.groupId.get
        obsInGroup1  <- queryObservationsInGroup(groupId)
        telluricOid  =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
        telluricTgt  <- queryObservationWithTarget(telluricOid)
        telluricTid  =  telluricTgt.targetId.get
        _            <- set(oid)
        _            <- recalculateCalibrations(pid, when)
        groupExists  <- queryGroupExists(groupId)
        tellExists   <- queryObservationExists(telluricOid)
        targetExists <- queryTargetExists(telluricTid)
      } yield {
        assertEquals(obsInGroup1.size, 2)
        assert(!groupExists)
        assert(!tellExists)
        assert(!targetExists)
      }

  test("Generate for defined and ready observations"):
    List(ObservationWorkflowState.Defined, ObservationWorkflowState.Ready).traverse_ : state =>
      for {
        pid         <- createProgramAs(pi)
        tid         <- createTargetWithProfileAs(pi, pid)
        oid         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
        _           <- setCalculatedWorkflowState(oid, state)
        _           <- runObscalcUpdate(pid, oid)
        _           <- recalculateCalibrations(pid, when)
        obs         <- queryObservation(oid)
        obsInGroup  <- obs.groupId.traverse(queryObservationsInGroup)
      } yield {
        // Defined and Ready observations should generate telluric calibrations
        assert(obs.groupId.isDefined)
        assertEquals(obsInGroup.map(_.size), Some(2))
      }

  test("don't delete ongoing and completed calibrations"):
    List(ObservationWorkflowState.Ongoing, ObservationWorkflowState.Completed).traverse_ : state =>
      for {
        pid                  <- createProgramAs(pi)
        tid                  <- createTargetWithProfileAs(pi, pid)
        oid                  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
        _                    <- runObscalcUpdate(pid, oid)
        (added1, removed1)   <- recalculateCalibrations(pid, when)
        obs1                 <- queryObservation(oid)
        groupId              =  obs1.groupId.get
        obsInGroup1          <- queryObservationsInGroup(groupId)
        telluricOid          =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
        _                    <- setCalculatedWorkflowState(telluricOid, state)
        _                    <- updateFlamingos2Fpu(oid, Flamingos2Fpu.LongSlit2)
        (added2, removed2)   <- recalculateCalibrations(pid, when)
        obsInGroup2          <- queryObservationsInGroup(groupId)
        telluricExists       <- queryObservationExists(telluricOid)
      } yield {
        // Telluric calibration in Ongoing or Completed state should be preserved
        assertEquals(obsInGroup2.size, 2)
        assert(telluricExists)
        assert(obsInGroup2.exists(_.id === telluricOid))
        assertEquals(added1.size, 1)
        assertEquals(removed1.size, 0)
        assertEquals(added2.size, 0)
        assertEquals(removed2.size, 0)
      }

  test("don't delete ongoing tellurics when recreating multiple tellurics"):
    List(ObservationWorkflowState.Ongoing, ObservationWorkflowState.Completed).traverse_ : state =>
      for {
        pid                  <- createProgramAs(pi)
        tid                  <- createTargetWithProfileAs(pi, pid)
        oid                  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
        _                    <- setExposureTime(oid, 120)
        _                    <- runObscalcUpdate(pid, oid)
        (added1, removed1)   <- recalculateCalibrations(pid, when)
        obs1                 <- queryObservation(oid)
        groupId              =  obs1.groupId.get
        obsInGroup1          <- queryObservationsInGroup(groupId)
        telluricOids         =  obsInGroup1.filter(_.calibrationRole.contains(CalibrationRole.Telluric)).map(_.id)
        firstTelluricOid     =  telluricOids.head
        _                    <- setCalculatedWorkflowState(firstTelluricOid, state)
        _                    <- updateFlamingos2Fpu(oid, Flamingos2Fpu.LongSlit2)
        _                    <- runObscalcUpdate(pid, oid)
        (added2, removed2)   <- recalculateCalibrations(pid, when)
        obsInGroup2          <- queryObservationsInGroup(groupId)
        telluricExists       <- queryObservationExists(firstTelluricOid)
      } yield {
        // First telluric in Ongoing/Completed state should be preserved
        assertEquals(telluricOids.size, 2)
        assert(telluricExists)
        assert(obsInGroup2.exists(_.id === firstTelluricOid))
        // Count unchanged (2), config synced on deletable telluric
        assertEquals(obsInGroup2.filter(_.calibrationRole.contains(CalibrationRole.Telluric)).size, 2)
        assertEquals(obsInGroup2.size, 3) // 2 tellurics + 1 science
        assertEquals(added1.size, 2)
        assertEquals(removed1.size, 0)
        assertEquals(added2.size, 0) // No count change, just config sync
        assertEquals(removed2.size, 0)
      }

  test("telluric group rejects second science observation"):
    for {
      pid   <- createProgramAs(pi)
      tid   <- createTargetWithProfileAs(pi, pid)
      oid1  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _     <- runObscalcUpdate(pid, oid1)
      _     <- recalculateCalibrations(pid, when)
      obs1  <- queryObservation(oid1)
      gid   =  obs1.groupId.get
      oid2  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      err   <- moveObservationAs(serviceUser, oid2, Some(gid)).intercept[UnexpectedStatus]
    } yield
      assertEquals(err.status.code, 500)

  test("telluric group rejects creating child groups"):
    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _    <- runObscalcUpdate(pid, oid)
      _    <- recalculateCalibrations(pid, when)
      obs  <- queryObservation(oid)
      gid  =  obs.groupId.get
      err  <- createGroupAs(pi, pid, parentGroupId = Some(gid)).intercept[UnexpectedStatus]
    yield
      assertEquals(err.status.code, 500)

  test("telluric target gets SED from telluric type when not set"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetWithProfileAs(pi, pid)
      oid         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _           <- setScienceRequirements(oid)
      _           <- runObscalcUpdate(pid, oid)
      _           <- recalculateCalibrations(pid, when)
      _           <- sleep >> resolveTelluricTargets
      obs         <- queryObservation(oid)
      groupId     =  obs.groupId.get
      obsInGroup  <- queryObservationsInGroup(groupId)
      telluricOid =  obsInGroup.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricObs <- queryObservationWithTarget(telluricOid)
      targetSed   <- telluricObs.targetId.flatTraverse(queryTargetSed)
    } yield {
      // Mock star has TelluricType.A0V => SED should A0V
      assertEquals(targetSed, Some("A0_V"))
    }

  test("telluric resolution uses science observation duration"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      obs1         <- queryObservation(oid)
      groupId      =  obs1.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      _            <- setScienceRequirements(oid)
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      _            <- sleep >> resolveTelluricTargets
      obs          <- queryObservationWithTarget(telluricOid)
      obscalcDur   <- withServicesForObscalc(serviceUser): services =>
                        services.transactionally:
                          services.obscalcService.selectOne(oid).map:
                            _.flatMap(_.result)
                              .flatMap(_.digest)
                              .map(d => d.science.timeEstimate.programTime |+| d.science.timeEstimate.nonCharged)
      storedDur    <- selectMeta(telluricOid).map(_.map(_.scienceDuration))
    } yield {
      assert(obs.targetName.isDefined)
      assertEquals(storedDur, obscalcDur)
    }

  test("coordinate change triggers re-resolution with new hash"):
    val obsTime = Timestamp.fromInstantTruncated(when).get

    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- setScienceRequirements(oid)
      _            <- setObservationTimeAndDuration(pi, oid, obsTime.some, none)
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      _            <- sleep >> resolveTelluricTargets
      obs1         <- queryObservation(oid)
      groupId      =  obs1.groupId.get
      obsInGroup   <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      hash1        <- selectParamsHash(telluricOid)
      meta1        <- selectMeta(telluricOid)
      _            <- updateTargetPropertiesAs(pi, tid, Coordinates.Zero)
      _            <- runObscalcUpdate(pid, oid)
      _            <- sleep >> resolveTelluricTargets
      hash2        <- selectParamsHash(telluricOid)
      meta2        <- selectMeta(telluricOid)
    } yield {
      assert(hash1.isDefined)
      assert(meta1.exists(_.resolvedTargetId.isDefined))
      assertNotEquals(hash1, hash2)
      // The should have different ids but our mock service returns always the same
      assert(meta2.exists(_.resolvedTargetId.isDefined))
    }

  private def setExposureTime(oid: Observation.Id, totalMinutes: Int): IO[Unit] =
    val perExposureMinutes = totalMinutes / 6
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: {
            observingMode: {
              flamingos2LongSlit: {
                exposureTimeMode: {
                  timeAndCount: {
                    time: { minutes: $perExposureMinutes },
                    count: 6,
                    at: { nanometers: 1390 }
                  }
                }
              }
            }
          }
        }) {
          observations { id }
        }
      }"""
    ).void

  private def queryObservationDuration(oid: Observation.Id): IO[Option[TimeSpan]] =
    withServicesForObscalc(serviceUser): services =>
      services.transactionally:
        services.obscalcService.selectExecutionDigest(oid).map:
          _.flatMap(_.value.toOption)
            .map(d => d.science.timeEstimate.sum |+| d.science.timeEstimate.nonCharged)

  test("create two tellurics for long science"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- setExposureTime(oid, 120)
      _                  <- runObscalcUpdate(pid, oid)
      duration           <- queryObservationDuration(oid)
      (added, removed)   <- recalculateCalibrations(pid, when)
      obs                <- queryObservation(oid)
      groupId            =  obs.groupId.get
      obsInGroup         <- queryObservationsInGroup(groupId)
      telluricObs        =  obsInGroup.filter(_.calibrationRole.contains(CalibrationRole.Telluric))
      scienceObs         =  obsInGroup.filter(_.calibrationRole.isEmpty).head
      metas              <- added.traverse(selectMeta)
      orders             =  metas.flatten.map(_.calibrationOrder)
    } yield {
      assertEquals(telluricObs.size, 2)
      assertEquals(obsInGroup.size, 3)
      assertEquals(added.size, 2)
      assertEquals(removed.size, 0)
      // Verify tellurics positions in the group
      assertEquals(telluricObs.head.groupIndex.get, scienceObs.groupIndex.get - 1)
      assertEquals(telluricObs.last.groupIndex.get, scienceObs.groupIndex.get + 1)
      assert(orders.contains(TelluricCalibrationOrder.Before))
      assert(orders.contains(TelluricCalibrationOrder.After))
    }

  test("create one telluric for short duration science observation"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- setExposureTime(oid, 60)
      _                  <- runObscalcUpdate(pid, oid)
      (added, removed)   <- recalculateCalibrations(pid, when)
      obs                <- queryObservation(oid)
      groupId            =  obs.groupId.get
      obsInGroup         <- queryObservationsInGroup(groupId)
      telluricObs        =  obsInGroup.filter(_.calibrationRole.contains(CalibrationRole.Telluric))
      scienceObs         =  obsInGroup.filter(_.calibrationRole.isEmpty).head
      meta               <- selectMeta(added.head)
    } yield {
      assertEquals(telluricObs.size, 1)
      assertEquals(obsInGroup.size, 2)
      assertEquals(added.size, 1)
      assertEquals(removed.size, 0)
      // Verify tellurics positions in the group
      assertEquals(telluricObs.head.groupIndex.get, scienceObs.groupIndex.get + 1)
      assertEquals(meta.get.calibrationOrder, TelluricCalibrationOrder.After)
    }

  test("update from two to one telluric when duration decreases"):
    for {
      pid                  <- createProgramAs(pi)
      tid                  <- createTargetWithProfileAs(pi, pid)
      oid                  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                    <- setExposureTime(oid, 120)
      _                    <- runObscalcUpdate(pid, oid)
      (added1, removed1)   <- recalculateCalibrations(pid, when)
      obs1                 <- queryObservation(oid)
      groupId              =  obs1.groupId.get
      obsInGroup1          <- queryObservationsInGroup(groupId)
      telluricObs1         =  obsInGroup1.filter(_.calibrationRole.contains(CalibrationRole.Telluric))
      // Change to short duration
      _                    <- setExposureTime(oid, 60)
      _                    <- runObscalcUpdate(pid, oid)
      (added2, removed2)   <- recalculateCalibrations(pid, when)
      obsInGroup2          <- queryObservationsInGroup(groupId)
      telluricObs2         =  obsInGroup2.filter(_.calibrationRole.contains(CalibrationRole.Telluric))
    } yield {
      // first we have 2 tellurics
      assertEquals(telluricObs1.size, 2)
      assertEquals(added1.size, 2)
      assertEquals(removed1.size, 0)
      // As duration is shorter we only need one
      assertEquals(telluricObs2.size, 1)
      assertEquals(added2.size, 1)
      assertEquals(removed2.size, 2)
    }

  test("Telluric group and obs preserved when science becomes ongoing"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- runObscalcUpdate(pid, oid)
      (added1, removed1) <- recalculateCalibrations(pid, when)
      _                  <- sleep >> resolveTelluricTargets
      obsBefore          <- queryObservation(oid)
      groupId            =  obsBefore.groupId.get
      obsInGroup1        <- queryObservationsInGroup(groupId)
      telluricOid        =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      // Set science observation to Ongoing
      _                  <- setCalculatedWorkflowState(oid, ObservationWorkflowState.Ongoing)
      (added2, removed2) <- recalculateCalibrations(pid, when)
      groupExists        <- queryGroupExists(groupId)
      tellExists         <- queryObservationExists(telluricOid)
      obsInGroup2        <- queryObservationsInGroup(groupId)
    } yield {
      assertEquals(obsInGroup1.size, 2)
      assertEquals(added1.size, 1)
      assertEquals(removed1.size, 0)
      // After science becomes Ongoing the group remains as well
      // as the exisiting telluric
      // This was broken and the telluric obs and group would be deleted
      assert(groupExists)
      assert(tellExists)
      assertEquals(obsInGroup2.size, 2)
    }

  test("Telluric group and obs preserved when science becomes observed"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- runObscalcUpdate(pid, oid)
      (added1, removed1) <- recalculateCalibrations(pid, when)
      _                  <- sleep >> resolveTelluricTargets
      obsBefore          <- queryObservation(oid)
      groupId            =  obsBefore.groupId.get
      obsInGroup1        <- queryObservationsInGroup(groupId)
      telluricOid        =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      // Set science observation to Complete
      _                  <- setCalculatedWorkflowState(oid, ObservationWorkflowState.Completed)
      (added2, removed2) <- recalculateCalibrations(pid, when)
      groupExists        <- queryGroupExists(groupId)
      tellExists         <- queryObservationExists(telluricOid)
      obsInGroup2        <- queryObservationsInGroup(groupId)
    } yield {
      assertEquals(obsInGroup1.size, 2)
      assertEquals(added1.size, 1)
      assertEquals(removed1.size, 0)
      // After science becomes Completed the group and telluric obs remain
      assert(groupExists)
      assert(tellExists)
      assertEquals(obsInGroup2.size, 2)
    }

  test("Telluric with visit is preserved when science becomes inactive"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- runObscalcUpdate(pid, oid)
      (added1, removed1) <- recalculateCalibrations(pid, when)
      obsBefore          <- queryObservation(oid)
      groupId            =  obsBefore.groupId.get
      obsInGroup1        <- queryObservationsInGroup(groupId)
      telluricOid        =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      // Record a visit
      _                  <- recordVisitAs(serviceUser, Instrument.Flamingos2, telluricOid)
      // science observation becomes Inactive, should not delete the telluric
      _                  <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Inactive)
      (added2, removed2) <- recalculateCalibrations(pid, when)
      groupExists        <- queryGroupExists(groupId)
      tellExists         <- queryObservationExists(telluricOid)
    } yield {
      assertEquals(added1.size, 1)
      assert(tellExists)
      assert(groupExists)
      assertEquals(removed2.size, 0)
    }

  test("Telluric with visit is preserved when science changes to GMOS"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- runObscalcUpdate(pid, oid)
      (added1, removed1) <- recalculateCalibrations(pid, when)
      obs1               <- queryObservation(oid)
      groupId            =  obs1.groupId.get
      obsInGroup1        <- queryObservationsInGroup(groupId)
      telluricOid        =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      // Record a visit on the telluric
      _                  <- recordVisitAs(serviceUser, Instrument.Flamingos2, telluricOid)
      // Change to GMOS
      _                  <- updateObservationMode(oid, "gmosNorthLongSlit")
      (added2, removed2) <- recalculateCalibrations(pid, when)
      groupExists        <- queryGroupExists(groupId)
      tellExists         <- queryObservationExists(telluricOid)
    } yield {
      assertEquals(added1.size, 1)
      assert(tellExists)
      assert(groupExists)
      assertEquals(removed2.size, 0)
    }

  test("Telluric with visit is preserved when duration decreases"):
    for {
      pid                  <- createProgramAs(pi)
      tid                  <- createTargetWithProfileAs(pi, pid)
      oid                  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                    <- setExposureTime(oid, 120)
      _                    <- runObscalcUpdate(pid, oid)
      (added1, removed1)   <- recalculateCalibrations(pid, when)
      obs1                 <- queryObservation(oid)
      groupId              =  obs1.groupId.get
      obsInGroup1          <- queryObservationsInGroup(groupId)
      telluricObs1         =  obsInGroup1.filter(_.calibrationRole.contains(CalibrationRole.Telluric))
      telluricOids1        =  telluricObs1.map(_.id).toSet
      // Record visits on both tellurics
      _                    <- telluricObs1.traverse_(t => recordVisitAs(serviceUser, Instrument.Flamingos2, t.id))
      // Change to short duration
      _                    <- setExposureTime(oid, 60)
      _                    <- runObscalcUpdate(pid, oid)
      (added2, removed2)   <- recalculateCalibrations(pid, when)
      obsInGroup2          <- queryObservationsInGroup(groupId)
      telluricObs2         =  obsInGroup2.filter(_.calibrationRole.contains(CalibrationRole.Telluric))
      telluricOids2        =  telluricObs2.map(_.id).toSet
    } yield {
      // Initially have 2 tellurics
      assertEquals(telluricObs1.size, 2)
      assertEquals(added1.size, 2)
      assertEquals(removed1.size, 0)
      // Original tellurics should be preserved (not deleted)
      assert(telluricOids1.subsetOf(telluricOids2))
      assertEquals(removed2.size, 0)
    }

  test("telluric etm is sn with a max of 100"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- setScienceRequirements(oid, DefaultSnAt, 75.0)
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      scienceEtm   <- queryExposureTimeMode(oid)
      obs          <- queryObservation(oid)
      groupId      =  obs.groupId.get
      obsInGroup   <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricEtms <- queryAllEtms(telluricOid)
    } yield {
      assertEquals(scienceEtm.snValue, SignalToNoise.unsafeFromBigDecimalExact(75.0).some)
      assertEquals(scienceEtm.snWAt, Wavelength.fromIntNanometers(510))
      // Science has S/N = 75, telluric would be max(100, 2 * 75 = 150)
      assertEquals(telluricEtms.science.snValue, SignalToNoise.unsafeFromBigDecimalExact(100).some)
      assertEquals(telluricEtms.science.snWAt, scienceEtm.snWAt)
      assertEquals(telluricEtms.acquisition.snValue, SignalToNoise.unsafeFromBigDecimalExact(10).some)
      assertEquals(telluricEtms.acquisition.snWAt, Wavelength.fromIntNanometers(500))
    }

  test("telluric sv is 2x science"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- setScienceRequirements(oid, DefaultSnAt, 30.0)
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      scienceEtm   <- queryExposureTimeMode(oid)
      obs          <- queryObservation(oid)
      groupId      =  obs.groupId.get
      obsInGroup   <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricEtms <- queryAllEtms(telluricOid)
    } yield {
      // Science has sn = 30, telluric is 60
      assertEquals(scienceEtm.snValue, SignalToNoise.unsafeFromBigDecimalExact(30.0).some)
      assertEquals(scienceEtm.snWAt, Wavelength.fromIntNanometers(510))
      // Science etm is synced for tellurics
      assertEquals(telluricEtms.science.snValue, SignalToNoise.unsafeFromBigDecimalExact(60).some)
      assertEquals(telluricEtms.science.snWAt, scienceEtm.snWAt)
    }

  test("telluric gets sn 100 when science is txc"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- setScienceRequirementsTimeAndCount(oid)
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      scienceEtm   <- queryExposureTimeMode(oid)
      obs          <- queryObservation(oid)
      groupId      =  obs.groupId.get
      obsInGroup   <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricEtms <- queryAllEtms(telluricOid)
    } yield {
      assertEquals(scienceEtm.snValue, None)
      assert(scienceEtm.txcValue.isDefined)
      assertEquals(scienceEtm.txcWvAt, Wavelength.fromIntNanometers(1390))
      // Science etm is synced for tellurics
      assertEquals(telluricEtms.science.snValue, SignalToNoise.unsafeFromBigDecimalExact(100).some)
      assertEquals(telluricEtms.science.snWAt, scienceEtm.txcWvAt)
    }

  test("telluric etm is updated when science etm changes"):
    val wavelength1 = Wavelength.fromIntNanometers(500).get
    val wavelength2 = Wavelength.fromIntNanometers(600).get

    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- setScienceRequirements(oid, wavelength1, 40.0)
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      obs          <- queryObservation(oid)
      groupId      =  obs.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricEtms1 <- queryAllEtms(telluricOid)
      // Update science to higher sn
      _            <- setScienceRequirements(oid, wavelength2, 60.0)
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      telluricEtms2 <- queryAllEtms(telluricOid)
    } yield {
      assertEquals(telluricEtms1.science.snValue, SignalToNoise.unsafeFromBigDecimalExact(80).some)
      assertEquals(telluricEtms1.science.snWAt, wavelength1.some)
      assertEquals(telluricEtms2.science.snValue, SignalToNoise.unsafeFromBigDecimalExact(100).some)
      assertEquals(telluricEtms2.science.snWAt, wavelength2.some)
    }

  test("telluric etm is updated when science changes from sn to txc"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- setScienceRequirements(oid, DefaultSnAt, 40.0)
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      obs          <- queryObservation(oid)
      groupId      =  obs.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricEtms1 <- queryAllEtms(telluricOid)
      // science etm from sn to txc
      _            <- setScienceRequirementsTimeAndCount(oid, atNm = BigDecimal(1500))
      _            <- runObscalcUpdate(pid, oid)
      _            <- recalculateCalibrations(pid, when)
      telluricEtms2 <- queryAllEtms(telluricOid)
    } yield {
      assertEquals(telluricEtms1.science.snValue, SignalToNoise.unsafeFromBigDecimalExact(80).some)
      assertEquals(telluricEtms1.science.snWAt, Wavelength.fromIntNanometers(510))
      // Only Science ETM is synced for tellurics
      assertEquals(telluricEtms2.science.snValue, SignalToNoise.unsafeFromBigDecimalExact(100).some)
      assertEquals(telluricEtms2.science.snWAt, Wavelength.fromIntNanometers(1500))
    }
