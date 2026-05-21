// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2

import java.time.LocalDateTime
import java.time.ZoneOffset

// sc-8734: Duplicating a science observation inside a telluric group fails with
// "cannot be moved out of their system group".
// The code was trying to clone the observation in place. it is not allowed in a system group.
// With the fix, the clone is now placed in the parent of the system group instead.
//
class ShortCut_8734
  extends ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport:

  private val when =
    LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  test("duplicating a science observation inside a telluric group succeeds"):
    for {
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      oid       <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _         <- setExposureTime(oid, 120)
      _         <- recalculateCalibrations(pid, when, oid)
      obsBefore <- queryObservation(oid)
      tellGid   = obsBefore.groupId.get
      // Duplicate with an explicit move to top level, copied from explore.
      newOid    <- query(
                     user  = pi,
                     query = s"""
                       mutation {
                         cloneObservation(input: {
                           observationId: "$oid"
                           SET: { groupId: null }
                         }) {
                           newObservation { id }
                         }
                       }
                     """
                   ).map(_.hcursor.downFields("cloneObservation", "newObservation", "id").require[Observation.Id])
      newObs    <- queryObservation(newOid)
    } yield
      // Source remains in the telluric system group.
      assertEquals(obsBefore.groupId, tellGid.some)
      // Clone is moved out of the system group.
      assertEquals(newObs.groupId, None)
