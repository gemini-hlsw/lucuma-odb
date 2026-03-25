// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionQuerySetupOperations
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class updateObservations_deletion extends OdbSuite with UpdateObservationsOps with ExecutionTestSupportForGmos with ExecutionQuerySetupOperations:

  test("existence: can delete observation without events"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some)
      _   <- deleteObservation(pi, oid)
    yield ()

  test("existence: cannot soft-delete observation with visits"):
    recordAll(
      pi,
      serviceUser,
      ObservingModeType.GmosNorthLongSlit,
      offset = 10000,
      atomCount = 1,
      stepCount = 1,
      datasetCount = 1).flatMap: on =>
        expectOdbError(
          user = pi,
          query = s"""
            mutation {
              updateObservations(input: {
                SET: { existence: DELETED }
                WHERE: { id: { EQ: "${on.id}" } }
              }) {
                observations { id }
              }
            }
          """,
          expected = {
            case OdbError.InvalidObservation(_, _) => ()
          }
        )

  test("existence: cannot hard-delete calibration observation with visits"):
    for
      pid <- createProgramAs(serviceUser)
      tid <- createTargetAs(serviceUser, pid)
      oid <- createObservationAs(serviceUser, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      _   <- setObservationCalibrationRole(List(oid), CalibrationRole.SpectroPhotometric)
      vid <- recordVisitAs(serviceUser, lucuma.core.enums.Instrument.GmosNorth, oid)
      _   <- addSequenceEventAs(serviceUser, vid, lucuma.core.enums.SequenceCommand.Start)
      res <- withServices(serviceUser) { services =>
              services.transactionally:
                 services.observationService.deleteCalibrationObservations(NonEmptyList.one(oid))
             }
      _   <- res match
              case Result.Failure(ps) =>
                val msg = ps.map(_.message).head
                  if msg.contains("Cannot delete observation") && msg.contains("visit") then
                    IO.unit
                  else
                    IO.raiseError(new AssertionError(s"Expected deletion to fail with visit constraint, but got: $msg"))
              case r =>
                IO.raiseError(new AssertionError(s"Expected deletion to fail, but it succeeded ($r)"))
    yield ()

