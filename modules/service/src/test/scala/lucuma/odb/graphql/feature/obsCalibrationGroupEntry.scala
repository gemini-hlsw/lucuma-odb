// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2

import java.time.LocalDateTime
import java.time.ZoneOffset

// A per-observation calibration group holds exactly one science observation,
// which its calibrations follow, so observations can't be added to one,
// whether created there, moved in or cloned in.
class obsCalibrationGroupEntry
  extends ExecutionTestSupportForFlamingos2
  with TelluricCalibrationsTestSupport:

  private val when =
    LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  // A science observation in its telluric calibration group.
  private def telluricGroup: IO[(Program.Id, Observation.Id, Group.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _   <- setExposureTime(oid, 120)
      _   <- recalculateCalibrations(pid, when, oid)
      obs <- queryObservation(oid)
    yield (pid, oid, obs.groupId.get)

  private def rejected(gid: Group.Id): PartialFunction[OdbError, Unit] =
    case OdbError.InvalidArgument(Some(msg)) =>
      assertEquals(msg, s"Observations cannot be added to calibration group $gid.")

  test("cannot create an observation in a calibration group"):
    for
      (pid, _, gid) <- telluricGroup
      before        <- queryObservationsInGroup(gid)
      _             <- interceptOdbError(createObservationInGroupAs(pi, pid, gid.some))(rejected(gid))
      after         <- queryObservationsInGroup(gid)
    yield assertEquals(after, before)

  test("cannot move an observation into a calibration group"):
    for
      (pid, _, gid) <- telluricGroup
      before        <- queryObservationsInGroup(gid)
      other         <- createObservationAs(pi, pid)
      _             <- interceptOdbError(moveObservationAs(pi, other, gid.some))(rejected(gid))
      after         <- queryObservationsInGroup(gid)
      otherAfter    <- queryObservation(other)
    yield
      assertEquals(after, before)
      assertEquals(otherAfter.groupId, None)

  test("cannot clone a science observation back into its calibration group"):
    for
      (_, oid, gid) <- telluricGroup
      before        <- queryObservationsInGroup(gid)
      clone          = query(
                         user  = pi,
                         query = s"""
                           mutation {
                             cloneObservation(input: {
                               observationId: "$oid"
                               SET: { groupId: "$gid" }
                             }) {
                               newObservation { id }
                             }
                           }
                         """
                       )
      _             <- interceptOdbError(clone)(rejected(gid))
      after         <- queryObservationsInGroup(gid)
    yield assertEquals(after, before)
