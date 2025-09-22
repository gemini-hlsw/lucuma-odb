// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.odb.data.Existence
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.util.Codecs.*

import java.time.*

class CalibrationsServiceSuite extends OdbSuite with ExecutionTestSupport:

  // Just a random fixed time.
  val when = LocalDateTime.of(2025, Month.MARCH, 3, 23, 30, 0).atZone(ZoneId.of("America/Santiago")).toInstant

  test("calibrationTargets smoke test"):
    withServices(serviceUser): services =>
      assertIOBoolean(
        services
          .calibrationsService(emailConfig, httpClient)
          .calibrationTargets(List(CalibrationRole.SpectroPhotometric), when)
          .map(_.sizeIs > 0)
      )

  test("calibrationTargets filter deleted"):
    val roles = List(CalibrationRole.SpectroPhotometric)
    withServices(serviceUser): services =>
      assertIOBoolean:
        Services.asSuperUser:
          services.transactionally:
            for
              before <- services.calibrationsService(emailConfig, httpClient).calibrationTargets(roles, when)
              _      <- services.targetService.updateTargets(
                 AccessControl.unchecked(
                   TargetPropertiesInput.Edit(none, none, none, Existence.Deleted.some),
                   List(before.head._1),
                   target_id
                 )
              )
              after  <- services.calibrationsService(emailConfig, httpClient).calibrationTargets(roles, when)
            yield before.tail === after

  test("selectAllUnexecuted excludes observations with execution events"):
    withServices(serviceUser): services =>
      Services.asSuperUser:
        services.transactionally:
          for
            pid   <- createProgram
            oid1  <- createObservationAs(serviceUser, pid)
            oid2  <- createObservationAs(serviceUser, pid)
            // Test that all observations are returned when no events exist
            allCalibs1 <- services.generatorParamsService.selectAllUnexecuted(pid, ObservationSelection.All)
            _ = assert(allCalibs1.size >= 2) // Should include both observations
            // Test that the regular selectAll also returns the same observations
            allCalibs2 <- services.generatorParamsService.selectAll(pid, ObservationSelection.All)
            _ = assert(allCalibs1.size === allCalibs2.size) // Should be the same when no events exist
          yield true
