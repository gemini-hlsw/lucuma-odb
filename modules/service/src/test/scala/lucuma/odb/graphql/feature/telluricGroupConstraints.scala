// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.odb.data.Existence
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2
import org.http4s.client.UnexpectedStatus

import java.time.LocalDateTime
import java.time.ZoneOffset

class telluricGroupConstraints
  extends OdbSuite
  with ExecutionTestSupportForFlamingos2:

  val when = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  case class ObsInfo(
    id:              Observation.Id,
    groupId:         Option[Group.Id],
    calibrationRole: Option[CalibrationRole]
  ) derives Decoder

  private def queryObservation(oid: Observation.Id): IO[ObsInfo] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              id
              groupId
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
      c.hcursor.downField("group").focus.exists(!_.isNull)
    }

  test("telluric group rejects second science observation (NULL role)"):
    for {
      pid   <- createProgramAs(pi)
      tid   <- createTargetWithProfileAs(pi, pid)
      oid1  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _     <- runObscalcUpdate(pid, oid1)
      _     <- recalculateCalibrations(pid, when)
      obs1  <- queryObservation(oid1)
      gid   =  obs1.groupId.get
      // Create another observation (not in group yet)
      oid2  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      // Try to add second science obs to the telluric group - should fail with 500
      err   <- moveObservationAs(serviceUser, oid2, Some(gid)).intercept[UnexpectedStatus]
    } yield
      // Constraint violation causes 500 Internal Server Error
      assertEquals(err.status.code, 500)

  test("deleting science observation removes telluric group and calibrations"):
    for
      pid          <- createProgramAs(pi)
      tid          <- createTargetWithProfileAs(pi, pid)
      oid1         <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _            <- runObscalcUpdate(pid, oid1)
      _            <- recalculateCalibrations(pid, when)
      obs1         <- queryObservation(oid1)
      gid          =  obs1.groupId.get
      groupBefore  <- queryGroupExists(gid)
      // Delete the science observation
      _            <- setObservationExistence(pi, oid1, Existence.Deleted)
      // Re-run calibrations to trigger cleanup
      _            <- recalculateCalibrations(pid, when)
      // Verify the group is removed
      groupAfter   <- queryGroupExists(gid)
    yield
      assert(groupBefore, "Group should exist before deletion")
      assert(!groupAfter, "Group should be removed after recalculateCalibrations")

  test("telluric group rejects creating child groups"):
    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _    <- runObscalcUpdate(pid, oid)
      _    <- recalculateCalibrations(pid, when)
      obs  <- queryObservation(oid)
      gid  =  obs.groupId.get
      // Try to create a group inside the telluric group - should fail
      err  <- createGroupAs(pi, pid, parentGroupId = Some(gid)).intercept[UnexpectedStatus]
    yield
      assertEquals(err.status.code, 500)
