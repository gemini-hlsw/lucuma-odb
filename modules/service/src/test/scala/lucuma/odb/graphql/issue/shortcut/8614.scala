// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.issue.shortcut

import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2

import java.time.LocalDateTime
import java.time.ZoneOffset

// sc-8614: When a science observation within a telluric group becomes Ongoing
// during execution (typically because observe started executing it), the
// resulting events trigger a calibration recalculation.
//
// The science obs fails the filtering as the state is not yet marked as ongoing
// and the cleanup branch deleted the telluric group.
//
// This test verifies that the case has been fixed.
class ShortCut_8614
  extends ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport:

  private val when =
    LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  test("tellurics survive when science observation transitions to Ongoing"):
    for {
      pid                <- createProgramAs(pi)
      tid                <- createTargetWithProfileAs(pi, pid)
      oid                <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _                  <- setExposureTime(oid, 120)
      _                  <- runObscalcUpdate(pid, oid)
      (added1, removed1) <- recalculateCalibrations(pid, when, oid)
      obsBefore          <- queryObservation(oid)
      groupId             = obsBefore.groupId.get
      obsInGroup1        <- queryObservationsInGroup(groupId)
      telluricOids        = obsInGroup1.filter(_.calibrationRole.contains(CalibrationRole.Telluric)).map(_.id)
      // Force the science observation's workflow state to ongoing.
      _                  <- setCalculatedWorkflowState(oid, ObservationWorkflowState.Ongoing)
      (added2, removed2) <- recalculateCalibrations(pid, when, oid)
      groupExists        <- queryGroupExists(groupId)
      tellExists         <- telluricOids.traverse(queryObservationExists)
      obsInGroup2        <- queryObservationsInGroup(groupId)
    } yield
      assertEquals(telluricOids.size, 2)
      assertEquals(added1.size, 2)
      assertEquals(removed1.size, 0)
      assert(groupExists)
      // tellurics are not revomed
      assert(tellExists.forall(identity))
      assertEquals(obsInGroup2.size, 3) // 2 tellurics + 1 science
      assertEquals(removed2.size, 0)
