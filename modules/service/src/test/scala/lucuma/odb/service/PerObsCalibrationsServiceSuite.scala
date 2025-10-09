// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.service.CalibrationConfigSubset.Flamingos2Configs
import lucuma.odb.service.CalibrationConfigSubset.toConfigSubset
import lucuma.odb.service.CalibrationsService.ObsExtract

import java.time.*

class PerObsCalibrationsServiceSuite extends OdbSuite with ObservingModeSetupOperations:
  val serviceUser = TestUsers.service(nextId)
  lazy val validUsers = List(serviceUser)

  val when = LocalDateTime.of(2025, Month.MARCH, 3, 23, 30, 0).atZone(ZoneId.of("America/Santiago")).toInstant

  test("F2 tellurics: one telluric per science observation"):
    createProgramAs(serviceUser).flatMap: pid =>
      createTargetAs(serviceUser, pid).flatMap: tid =>
        createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: _ =>
          createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: _ =>
            createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: _ =>
              withServices(serviceUser): services =>
                assertIOBoolean:
                  Services.asSuperUser:
                    services.transactionally:
                      for
                        allSci <- services.generatorParamsService.selectAll(pid, ObservationSelection.Science)
                        f2Sci   = allSci.toList
                                    .collect { case (oid, Right(gp)) => (oid, gp.observingMode) }
                                    .filter(_._2.toConfigSubset.isInstanceOf[Flamingos2Configs])
                                    .map { case (oid, mode) =>
                                      ObsExtract(oid, none, none, none, mode.toConfigSubset)
                                    }
                        perObsService = PerObsCalibrationsService.instantiate(emailConfig, httpClient)
                        addedOids <- perObsService.generatePerObsCalibrations(pid, f2Sci)
                      yield addedOids.size == 3

  test("F2 tellurics: each in separate group"):
    createProgramAs(serviceUser).flatMap: pid =>
      createTargetAs(serviceUser, pid).flatMap: tid =>
        createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: _ =>
          createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: _ =>
            withServices(serviceUser): services =>
              assertIOBoolean:
                Services.asSuperUser:
                  services.transactionally:
                    for
                      allSci <- services.generatorParamsService.selectAll(pid, ObservationSelection.Science)
                      f2Sci   = allSci.toList
                                  .collect { case (oid, Right(gp)) => (oid, gp.observingMode) }
                                  .filter(_._2.toConfigSubset.isInstanceOf[Flamingos2Configs])
                                  .map { case (oid, mode) =>
                                    ObsExtract(oid, none, none, none, mode.toConfigSubset)
                                  }
                      perObsService = PerObsCalibrationsService.instantiate(emailConfig, httpClient)
                      _ <- perObsService.generatePerObsCalibrations(pid, f2Sci)
                      groups <- services.groupService(emailConfig, httpClient).selectGroups(pid)
                    yield
                      import lucuma.odb.data.GroupTree
                      val groupNames = groups match
                        case GroupTree.Root(_, children) =>
                          children.collect:
                            case GroupTree.Branch(_, _, _, _, Some(name), _, _, _, true) => name.value
                        case _ => List.empty
                      groupNames.count(_.contains("F2 Telluric for")) == 2

  test("F2 tellurics: idempotent - doesn't create duplicates"):
    createProgramAs(serviceUser).flatMap: pid =>
      createTargetAs(serviceUser, pid).flatMap: tid =>
        createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: _ =>
          withServices(serviceUser): services =>
            assertIOBoolean:
              Services.asSuperUser:
                services.transactionally:
                  for
                    allSci <- services.generatorParamsService.selectAll(pid, ObservationSelection.Science)
                    f2Sci   = allSci.toList
                                .collect { case (oid, Right(gp)) => (oid, gp.observingMode) }
                                .filter(_._2.toConfigSubset.isInstanceOf[Flamingos2Configs])
                                .map { case (oid, mode) =>
                                  ObsExtract(oid, none, none, none, mode.toConfigSubset)
                                }
                    perObsService = PerObsCalibrationsService.instantiate(emailConfig, httpClient)
                    first  <- perObsService.generatePerObsCalibrations(pid, f2Sci)
                    second <- perObsService.generatePerObsCalibrations(pid, f2Sci)
                  yield first.size == 1 && second.isEmpty
