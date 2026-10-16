// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError

class replaceDaytimeCalibrationSequence extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(pi, staff, serviceUser)

  private def replaceQuery(oid: Observation.Id, sequenceType: SequenceType, steps: String*): String =
    s"""
      mutation {
        replaceGmosNorthSequence(input: ${input(oid, sequenceType, atomInput("Darks", steps*))}) {
          sequence {
            description
            steps {
              observeClass
              stepConfig { stepType }
              telescopeConfig { guiding }
            }
          }
        }
      }
    """

  test("day calibration steps are stored with guiding disabled"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- expect(
        user     = staff,
        query    = replaceQuery(oid, SequenceType.Science, gmosNorthDarkStep(), gmosNorthDarkStep(guiding = Some("ENABLED"))),
        expected = json"""
          {
            "replaceGmosNorthSequence": {
              "sequence": [
                {
                  "description": "Darks",
                  "steps": [
                    {
                      "observeClass": "DAY_CAL",
                      "stepConfig": { "stepType": "DARK" },
                      "telescopeConfig": { "guiding": "DISABLED" }
                    },
                    {
                      "observeClass": "DAY_CAL",
                      "stepConfig": { "stepType": "DARK" },
                      "telescopeConfig": { "guiding": "DISABLED" }
                    }
                  ]
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("a step that is not a day calibration is rejected"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- expectOdbError(
        user     = staff,
        query    = replaceQuery(oid, SequenceType.Science, gmosNorthDarkStep(observeClass = "SCIENCE")),
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("DAY_CAL") => () }
      )
    yield ()

  test("an acquisition sequence is rejected"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- expectOdbError(
        user     = staff,
        query    = replaceQuery(oid, SequenceType.Acquisition, gmosNorthDarkStep()),
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("acquisition") => () }
      )
    yield ()

  test("defined once a sequence is entered"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- query(staff, replaceQuery(oid, SequenceType.Science, gmosNorthDarkStep()))
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- expect(
        user     = staff,
        query    = workflowQuery(oid),
        expected = json"""
          {
            "observation": {
              "workflow": {
                "value": {
                  "state": "DEFINED",
                  "validationErrors": []
                }
              }
            }
          }
        """.asRight
      )
    yield ()
