// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Program
import lucuma.odb.data.OdbError

class createObservation_DaytimeCalibration extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(pi, staff, serviceUser)

  private def createQuery(pid: Program.Id, instrument: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              daytimeCalibration: { instrument: $instrument }
            }
          }
        }) {
          observation {
            calibrationRole
            observingMode {
              mode
              instrument
              daytimeCalibration {
                mode
                instrument
              }
            }
          }
        }
      }
    """

  test("staff create a daytime calibration: mode, role and instrument are paired"):
    for
      pid <- createProgramAs(staff)
      _   <- expect(
        user     = staff,
        query    = createQuery(pid, "GMOS_NORTH"),
        expected = json"""
          {
            "createObservation": {
              "observation": {
                "calibrationRole": "DAYTIME_CALIBRATION",
                "observingMode": {
                  "mode": "DAYTIME_CALIBRATION",
                  "instrument": "GMOS_NORTH",
                  "daytimeCalibration": {
                    "mode": "DAYTIME_CALIBRATION",
                    "instrument": "GMOS_NORTH"
                  }
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("a PI may not create a daytime calibration"):
    for
      pid <- createProgramAs(pi)
      _   <- expectOdbError(
        user     = pi,
        query    = createQuery(pid, "GMOS_NORTH"),
        expected = { case OdbError.NotAuthorized(_, _) => () }
      )
    yield ()

  test("an unsupported instrument is rejected"):
    for
      pid <- createProgramAs(staff)
      _   <- expectOdbError(
        user     = staff,
        query    = createQuery(pid, "GHOST"),
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("does not support daytime calibrations") => () }
      )
    yield ()

  test("undefined until a sequence is entered"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- expect(
        user     = staff,
        query    = workflowQuery(oid),
        expected = json"""
          {
            "observation": {
              "workflow": {
                "value": {
                  "state": "UNDEFINED",
                  "validationErrors": [
                    { "messages": ["No sequence has been entered."] }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()
