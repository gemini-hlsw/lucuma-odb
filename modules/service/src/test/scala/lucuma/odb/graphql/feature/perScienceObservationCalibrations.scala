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
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.graphql.subscription.SubscriptionUtils
import lucuma.odb.json.time.transport.given

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

  case class GroupInfo(
    id: Group.Id,
    system: Boolean,
    name: String,
    ordered: Boolean,
    minimumRequired: Option[Int],
    maximumInterval: Option[TimeSpan],
    calibrationRoles: List[CalibrationRole]
  ) derives Decoder
  case class ObsInfo(id: Observation.Id, groupId: Option[Group.Id]) derives Decoder

  private def queryGroup(gid: Group.Id): IO[GroupInfo] =
    query(
      service,
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

  private def queryAllGroups(pid: Program.Id): IO[List[Group.Id]] =
    case class GroupWrapper(id: Group.Id) derives Decoder
    case class GroupElementWrapper(group: Option[GroupWrapper]) derives Decoder
    query(
      service,
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

  test("telluric group has immediate execution properties"):
    for {
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      oid       <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
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
      oid2        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      _           <- recalculateCalibrations(pid, when)
      obs1Before  <- queryObservation(oid1)
      obs2Before  <- queryObservation(oid2)
      group1Id    =  obs1Before.groupId.get
      // Delete obs1, keep obs2, add obs3
      _           <- setObservationInactive(oid1)
      oid3        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid3))
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
      _           <- recalculateCalibrations(pid, when)
      obs1Before  <- queryObservation(oid1)
      group1Id    =  obs1Before.groupId.get
      // Set observation1 inactive
      _           <- setObservationInactive(oid1)
      // Create new F2 observation
      oid2        <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
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
