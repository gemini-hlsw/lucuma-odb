// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.daytimecal.StaticConfigs
import lucuma.odb.service.Services.Syntax.*

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

  test("GHOST static has no IFU target"):
    def replaceGhost(oid: Observation.Id): String =
      s"""
        mutation {
          replaceGhostSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", ghostDarkStep))}) {
            sequence { description }
          }
        }
      """

    def executionQuery(oid: Observation.Id): String =
      s"""
        query {
          executionConfig(observationId: ${oid.asJson}) {
            ghost {
              static {
                resolutionMode
                ifuMapping { mappingType singleTarget { ifu1 } }
                slitViewingCameraExposureTime { seconds }
              }
              science {
                nextAtom {
                  steps { observeClass }
                }
              }
            }
          }
        }
      """

    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid, Instrument.Ghost)
      _   <- query(staff, replaceGhost(oid))
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""
          {
            "executionConfig": {
              "ghost": {
                "static": {
                  "resolutionMode": "STANDARD",
                  "ifuMapping": { "mappingType": "NO_TARGET", "singleTarget": null },
                  "slitViewingCameraExposureTime": null
                },
                "science": {
                  "nextAtom": {
                    "steps": [
                      { "observeClass": "DAY_CAL" }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  // Materialization skips daytime calibrations, so write the static directly to
  // exercise the no_target codec and CHECK.
  test("GHOST NO_TARGET static round trips through the database"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid, Instrument.Ghost)
      res <- withServices(serviceUser): services =>
               services.transactionally:
                 ghostSequenceService.insertStatic(oid, StaticConfigs.Ghost) *>
                 ghostSequenceService.selectStatic(oid)
    yield assertEquals(res, Some(StaticConfigs.Ghost))

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
