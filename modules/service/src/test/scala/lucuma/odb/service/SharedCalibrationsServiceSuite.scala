// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.CalibrationsService.ObsExtract

import java.time.*

class SharedCalibrationsServiceSuite extends OdbSuite with ObservingModeSetupOperations:
  val serviceUser = TestUsers.service(nextId)
  lazy val validUsers = List(serviceUser)

  val when = LocalDateTime.of(2025, Month.MARCH, 3, 23, 30, 0).atZone(ZoneId.of("America/Santiago")).toInstant

  test("GMOS shared calibrations: deduplicates by config"):
    createProgramAs(serviceUser).flatMap: pid =>
      createTargetWithProfileAs(serviceUser, pid).flatMap: tid =>
        createGmosNorthLongSlitObservationAs(serviceUser, pid, List(tid)).flatMap: _ =>
          createGmosNorthLongSlitObservationAs(serviceUser, pid, List(tid)).flatMap: _ =>
            withServices(serviceUser): services =>
              assertIOBoolean:
                Services.asSuperUser:
                  services.transactionally:
                    for
                      calibTargets <- services.calibrationsService(emailConfig, httpClient)
                                        .calibrationTargets(List(CalibrationRole.SpectroPhotometric), when)
                      allSci <- services.generatorParamsService.selectAll(pid, ObservationSelection.Science)
                      scienceObs = allSci.toList
                                     .collect { case (oid, Right(GeneratorParams(itc, band, mode, _, _, _))) =>
                                       ObsExtract(oid, itc.toOption, band, none, mode)
                                     }
                      sharedService = SharedCalibrationsService.instantiate(emailConfig, httpClient)
                      (added, _) <- sharedService.generateSharedCalibrations(
                                      pid,
                                      scienceObs,
                                      List.empty,
                                      calibTargets,
                                      when
                                    )
                    yield
                      // Two GMOS observations with same config should create only 1 calibration
                      added.size == 1
