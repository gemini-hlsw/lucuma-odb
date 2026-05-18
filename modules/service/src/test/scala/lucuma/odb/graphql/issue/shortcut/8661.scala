// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.issue.shortcut

import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2

import java.time.LocalDateTime
import java.time.ZoneOffset

// A system telluric group could be left orphaned (a "phantom group")
// when the parent science observation is moved out of the telluric group.
//
// It was reported that sometimes dragging and dropping created phantom
// groups and that could explain how these groups were left without a science obs.
class ShortCut_8661
  extends ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport:

  private val when =
    LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  test("cannot move a science observation out of a telluric group"):
    for {
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      oid       <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _         <- setExposureTime(oid, 120)
      _         <- recalculateCalibrations(pid, when, oid)
      obsBefore <- queryObservation(oid)
      tellGid   = obsBefore.groupId.get
      members   <- queryObservationsInGroup(tellGid)
      // Try to move the the science into another group
      newGid    <- createGroupAs(pi, pid, name = "Butterfly".some)
      _         <- interceptOdbError(moveObservationAs(pi, oid, newGid.some)):
                     case OdbError.InvalidArgument(Some(msg)) =>
                       assertEquals(
                         msg,
                         s"Observations $oid cannot be moved out of their system group; move the group instead."
                       )
      // The science obs remains on the telluric group.
      obsAfter  <- queryObservation(oid)
    } yield
      assert(members.exists(_.calibrationRole.contains(CalibrationRole.Telluric)))
      assertEquals(obsAfter.groupId, tellGid.some)
