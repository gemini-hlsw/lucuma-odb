// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError

class updateObservations_DaytimeCalibration extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(pi, staff, serviceUser)

  private def updateQuery(oid: Observation.Id, observingMode: String): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: { observingMode: $observingMode }
          WHERE: { id: { EQ: ${oid.asJson} } }
        }) {
          observations {
            calibrationRole
            observingMode {
              mode
              instrument
              daytimeCalibration { instrument }
            }
          }
        }
      }
    """

  private val gmosSouth: String =
    "{ daytimeCalibration: { instrument: GMOS_SOUTH } }"

  test("the instrument may change while no sequence is stored"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- expect(
        user     = staff,
        query    = updateQuery(oid, gmosSouth),
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "calibrationRole": "DAYTIME_CALIBRATION",
                  "observingMode": {
                    "mode": "DAYTIME_CALIBRATION",
                    "instrument": "GMOS_SOUTH",
                    "daytimeCalibration": { "instrument": "GMOS_SOUTH" }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("the instrument is fixed once a sequence is stored"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- query(staff, s"""
               mutation {
                 replaceGmosNorthSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", gmosNorthDarkStep()))}) {
                   sequence { id }
                 }
               }
             """)
      _   <- expectOdbError(
        user     = staff,
        query    = updateQuery(oid, gmosSouth),
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("Delete the sequence first") => () }
      )
    yield ()

  test("a PI may not edit a daytime calibration's mode"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- expectOdbError(
        user     = pi,
        query    = updateQuery(oid, gmosSouth),
        expected = { case OdbError.NotAuthorized(_, _) => () }
      )
    yield ()
