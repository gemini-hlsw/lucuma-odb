// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Wavelength
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.graphql.subscription.SubscriptionUtils

import java.time.LocalDateTime
import java.time.ZoneOffset

class perScienceObservationCalibrations
  extends OdbSuite
  with SubscriptionUtils
  with ExecutionTestSupport
  with ObservingModeSetupOperations:

  override val pi = TestUsers.Standard.pi(1, 101)
  val service     = TestUsers.service(3)

  val DefaultSnAt: Wavelength = Wavelength.fromIntNanometers(510).get

  override val validUsers = List(pi, service)

  val when = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  case class GroupInfo(id: Group.Id, system: Boolean, name: String, calibrationRoles: List[CalibrationRole]) derives Decoder
  case class ObsInfo(id: Observation.Id, groupId: Option[Group.Id]) derives Decoder
  case class ObservationWithRole(
    id: Observation.Id,
    calibrationRole: Option[CalibrationRole]
  ) derives Decoder
  case class ObsWithTarget(
    id: Observation.Id,
    targetId: Option[lucuma.core.model.Target.Id],
    targetName: Option[String],
    targetRa: Option[String],
    targetDec: Option[String]
  ) derives Decoder

  private def queryObservationsInGroup(gid: Group.Id): IO[List[ObservationWithRole]] =
    query(
      service,
      s"""query {
            group(groupId: "$gid") {
              elements {
                observation {
                  id
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
        .flatMap(_.hcursor.downField("observation").as[Option[ObservationWithRole]].toOption)
        .flatten
      elements.pure[IO]
    }

  private def queryObservationWithTarget(oid: Observation.Id): IO[ObsWithTarget] =
    query(
      service,
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
      service,
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
      service,
      s"""query {
            target(targetId: "$tid") {
              id
            }
          }"""
    ).map { c =>
      c.hcursor.downField("target").focus.exists(!_.isNull)
    }

  private def queryGroup(gid: Group.Id): IO[GroupInfo] =
    query(
      service,
      s"""query {
            group(groupId: "$gid") {
              id
              system
              name
              calibrationRoles
            }
          }"""
    ).flatMap { c =>
      c.hcursor.downField("group").as[GroupInfo]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  private def queryObservation(oid: Observation.Id): IO[ObsInfo] =
    query(
      service,
      s"""query {
            observation(observationId: "$oid") {
              id
              groupId
            }
          }"""
    ).flatMap { c =>
      c.hcursor.downField("observation").as[ObsInfo]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  private def queryGroupExists(gid: Group.Id): IO[Boolean] =
    query(
      service,
      s"""query {
            group(groupId: "$gid") {
              id
            }
          }"""
    ).map { c =>
      c.hcursor.downField("group").as[GroupInfo].isRight
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
      oid2  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
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

  test("F2 observation gets a telluric calibration observation"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetWithProfileAs(pi, pid)
      oid         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _           <- recalculateCalibrations(pid, when)
      obs         <- queryObservation(oid)
      groupId     =  obs.groupId.get
      obsInGroup  <- queryObservationsInGroup(groupId)
      telluricObs =  obsInGroup.filter(_.calibrationRole.contains(CalibrationRole.Telluric))
    } yield {
      assertEquals(telluricObs.size, 1, "Should have exactly one telluric observation")
      assertEquals(obsInGroup.size, 2, "Group should contain science obs + telluric obs")
    }

  test("telluric observation has placeholder target with correct properties"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetWithProfileAs(pi, pid)
      oid         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _           <- recalculateCalibrations(pid, when)
      obs         <- queryObservation(oid)
      groupId     =  obs.groupId.get
      obsInGroup  <- queryObservationsInGroup(groupId)
      telluricOid =  obsInGroup.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricObs <- queryObservationWithTarget(telluricOid)
    } yield {
      assertEquals(telluricObs.targetName, Some("Telluric Target (TBD)"))
      assertEquals(telluricObs.targetRa, Some("00:00:00.000000"))
      assertEquals(telluricObs.targetDec, Some("+00:00:00.000000"))
    }

  test("each F2 observation gets its own unique telluric observation and target"):
    for {
      pid          <- createProgramAs(pi)
      tid1         <- createTargetWithProfileAs(pi, pid)
      tid2         <- createTargetWithProfileAs(pi, pid)
      oid1         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      oid2         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      _            <- recalculateCalibrations(pid, when)
      obs1         <- queryObservation(oid1)
      obs2         <- queryObservation(oid2)
      obsInGroup1  <- queryObservationsInGroup(obs1.groupId.get)
      obsInGroup2  <- queryObservationsInGroup(obs2.groupId.get)
      telluric1Oid =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluric2Oid =  obsInGroup2.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluric1    <- queryObservationWithTarget(telluric1Oid)
      telluric2    <- queryObservationWithTarget(telluric2Oid)
    } yield {
      assertNotEquals(telluric1Oid, telluric2Oid, "Should have different telluric observations")
      assertNotEquals(telluric1.targetId, telluric2.targetId, "Should have different targets")
    }

  test("calling recalculateCalibrations multiple times is idempotent"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- recalculateCalibrations(pid, when)
      obs          <- queryObservation(oid)
      groupId      =  obs.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluric1Oid =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      _            <- recalculateCalibrations(pid, when)
      obsInGroup2  <- queryObservationsInGroup(groupId)
      telluric2Oid =  obsInGroup2.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
    } yield {
      assertEquals(obsInGroup2.size, 2, "Should still have exactly 2 observations")
      assertEquals(telluric1Oid, telluric2Oid, "Telluric obs ID should be preserved (synced, not recreated)")
    }

  test("recalculation syncs telluric observation configuration"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- recalculateCalibrations(pid, when)
      obs1         <- queryObservation(oid)
      groupId      =  obs1.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluric1Oid =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      _            <- recalculateCalibrations(pid, when)
      obsInGroup2  <- queryObservationsInGroup(groupId)
      telluric2Oid =  obsInGroup2.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
    } yield {
      assertEquals(obsInGroup2.size, 2, "Should still have exactly 2 observations")
      assertEquals(telluric1Oid, telluric2Oid, "Telluric obs should maintain same ID when synced")
    }

  test("changing F2 to GMOS deletes telluric observation"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- recalculateCalibrations(pid, when)
      obs1         <- queryObservation(oid)
      groupId      =  obs1.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      _            <- updateObservationMode(oid, "gmosNorthLongSlit")
      _            <- recalculateCalibrations(pid, when)
      obsExists    <- queryObservationExists(telluricOid)
      groupExists  <- queryGroupExists(groupId)
    } yield {
      assert(!obsExists, "Telluric observation should be deleted")
      assert(!groupExists, "System group should be deleted")
    }

  test("deleting F2 science observation deletes telluric observation"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid          <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- recalculateCalibrations(pid, when)
      obs1         <- queryObservation(oid)
      groupId      =  obs1.groupId.get
      obsInGroup1  <- queryObservationsInGroup(groupId)
      telluricOid  =  obsInGroup1.find(_.calibrationRole.contains(CalibrationRole.Telluric)).get.id
      telluricTgt  <- queryObservationWithTarget(telluricOid)
      telluricTid  =  telluricTgt.targetId.get
      _            <- setObservationInactive(oid)
      _            <- recalculateCalibrations(pid, when)
      telluricExists <- queryObservationExists(telluricOid)
      groupExists    <- queryGroupExists(groupId)
      targetExists   <- queryTargetExists(telluricTid)
    } yield {
      assert(!telluricExists, "Telluric observation should be deleted")
      assert(!groupExists, "System group should be deleted")
      assert(!targetExists, "Orphaned telluric target should be cleaned up")
    }
