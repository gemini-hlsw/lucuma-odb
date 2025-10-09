// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.odb.data.Existence
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.util.Codecs.*

import java.time.*

class CalibrationsServiceSuite extends OdbSuite:
  val serviceUser = TestUsers.service(nextId)
  lazy val validUsers = List(serviceUser)

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

  test("F2 tellurics: one telluric per science observation"):
    createProgramAs(serviceUser).flatMap: pid =>
      createTargetAs(serviceUser, pid).flatMap: tid =>
        createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: obs1 =>
          createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: obs2 =>
            createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: obs3 =>
              withServices(serviceUser): services =>
                assertIOBoolean:
                  Services.asSuperUser:
                    services.transactionally:
                      services.calibrationsService(emailConfig, httpClient)
                        .recalculateCalibrations(pid, when)
                        .map: (added, removed) =>
                          added.size == 3 && removed.isEmpty

  test("F2 tellurics: each in separate group"):
    createProgramAs(serviceUser).flatMap: pid =>
      createTargetAs(serviceUser, pid).flatMap: tid =>
        createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: obs1 =>
          createFlamingos2LongSlitObservationAs(serviceUser, pid, tid).flatMap: obs2 =>
            withServices(serviceUser): services =>
              assertIOBoolean:
                Services.asSuperUser:
                  services.transactionally:
                    for
                      result <- services.calibrationsService(emailConfig, httpClient)
                                  .recalculateCalibrations(pid, when)
                      groups <- services.groupService(emailConfig, httpClient).selectGroups(pid)
                    yield
                      import lucuma.odb.data.GroupTree
                      val groupNames = groups match
                        case GroupTree.Root(_, children) =>
                          children.collect:
                            case GroupTree.Branch(_, _, _, _, Some(name), _, _, _, true) => name.value
                        case _ => List.empty
                      groupNames.count(_.contains("F2 Telluric for")) == 2
