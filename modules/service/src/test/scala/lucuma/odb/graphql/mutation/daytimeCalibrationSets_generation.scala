// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.OdbError

class daytimeCalibrationSets_generation extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val staff       = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(staff, serviceUser)

  private def setup(exposureSeconds: Int, xBin: String = "TWO", slit: Boolean = false): String =
    s"""
      gmosNorth: {
        exposure: { seconds: $exposureSeconds }
        readout: { xBin: $xBin, yBin: $xBin, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
        dtax: ZERO
        roi: FULL_FRAME
        ${if slit then """
          gratingConfig: { grating: R831_G5302, order: ONE, wavelength: { nanometers: 615 } }
          fpu: { builtin: LONG_SLIT_1_00 }
        """ else ""}
      }
    """

  private val mockupSets: String =
    List(
      s"{ kind: FLAT, explicitCount: 3, gcal: { continuum: QUARTZ_HALOGEN5, filter: ND10, shutter: OPEN }, ${setup(15, slit = true)} }",
      s"{ kind: ARC, explicitCount: 5, gcal: { arcs: [AR_ARC], filter: ND10, shutter: CLOSED }, ${setup(10, slit = true)} }",
      s"{ kind: DARK, explicitCount: 10, ${setup(1)} }",
      s"{ kind: DARK, explicitCount: 10, ${setup(60)} }",
      s"{ kind: BIAS, explicitCount: 5, ${setup(0, "ONE")} }",
      s"{ kind: BIAS, explicitCount: 5, ${setup(0)} }"
    ).mkString(", ")

  private def createWithSets(pid: Program.Id, sets: String): IO[Observation.Id] =
    query(
      user  = staff,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              observingMode: {
                daytimeCalibration: { instrument: GMOS_NORTH, calibrationSets: [$sets] }
              }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  private val atomSelection: String =
    """
      description
      steps {
        observeClass
        stepConfig { stepType }
        telescopeConfig { guiding }
        instrumentConfig { exposure { seconds } readout { xBin } }
      }
    """

  private def executionQuery(oid: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: ${oid.asJson}) {
          gmosNorth {
            science {
              nextAtom { $atomSelection }
              possibleFuture { $atomSelection }
              hasMore
            }
          }
        }
      }
    """

  private def atom(kind: String, count: Int, stepType: String, seconds: Int, xBin: String = "TWO"): Json =
    val step = json"""
      {
        "observeClass": "DAY_CAL",
        "stepConfig": { "stepType": $stepType },
        "telescopeConfig": { "guiding": "DISABLED" },
        "instrumentConfig": { "exposure": { "seconds": ${BigDecimal(seconds).setScale(6)} }, "readout": { "xBin": $xBin } }
      }
    """
    json"""{ "description": ${s"$kind x$count"}, "steps": ${List.fill(count)(step)} }"""

  private val mockupAtoms: List[Json] =
    List(
      atom("Flat", 3,  "GCAL", 15),
      atom("Arc",  5,  "GCAL", 10),
      atom("Dark", 10, "DARK", 1),
      atom("Dark", 10, "DARK", 60),
      atom("Bias", 5,  "BIAS", 0, "ONE"),
      atom("Bias", 5,  "BIAS", 0)
    )

  private def expectedExecution(atoms: List[Json]): Json =
    json"""
      {
        "executionConfig": {
          "gmosNorth": {
            "science": {
              "nextAtom": ${atoms.head},
              "possibleFuture": ${atoms.tail},
              "hasMore": false
            }
          }
        }
      }
    """

  test("one atom per set, count steps each, in row order"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- expect(staff, executionQuery(oid), expectedExecution(mockupAtoms).asRight)
    yield ()

  test("no sets, no steps: empty sequence and Undefined"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""
          { "executionConfig": { "gmosNorth": { "science": null } } }
        """.asRight
      )
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
                  "validationErrors": [ { "messages": [ "No sequence has been defined." ] } ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("defined with sets, six atoms in the digest"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- expect(
        user     = staff,
        query    = s"""
          query {
            observation(observationId: ${oid.asJson}) {
              workflow { value { state validationErrors { messages } } }
              execution { digest { value { science { atomCount observeClass } } } }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "workflow": { "value": { "state": "DEFINED", "validationErrors": [] } },
              "execution": { "digest": { "value": { "science": { "atomCount": 6, "observeClass": "DAY_CAL" } } } }
            }
          }
        """.asRight
      )
    yield ()

  test("a hand-entered sequence takes precedence over the sets"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- query(staff, s"""
               mutation {
                 replaceGmosNorthSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", gmosNorthDarkStep()))}) {
                   sequence { id }
                 }
               }
             """)
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""
          {
            "executionConfig": {
              "gmosNorth": {
                "science": {
                  "nextAtom": {
                    "description": "Darks",
                    "steps": [
                      {
                        "observeClass": "DAY_CAL",
                        "stepConfig": { "stepType": "DARK" },
                        "telescopeConfig": { "guiding": "DISABLED" },
                        "instrumentConfig": { "exposure": { "seconds": 10.000000 }, "readout": { "xBin": "TWO" } }
                      }
                    ]
                  },
                  "possibleFuture": [],
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("the first visit materializes the sets and freezes them"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- recordVisitAs(serviceUser, oid)
      _   <- expect(staff, executionQuery(oid), expectedExecution(mockupAtoms).asRight)
      _   <- expectOdbError(
        user     = staff,
        query    = s"""
          mutation {
            updateObservations(input: {
              SET: { observingMode: { daytimeCalibration: { calibrationSets: [] } } }
              WHERE: { id: { EQ: ${oid.asJson} } }
            }) {
              observations { id }
            }
          }
        """,
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("Delete the sequence first") => () }
      )
    yield ()

  test("GMOS South generates too"):
    for
      pid <- createProgramAs(staff)
      oid <- query(staff, s"""
               mutation {
                 createObservation(input: {
                   programId: ${pid.asJson}
                   SET: {
                     observingMode: {
                       daytimeCalibration: {
                         instrument: GMOS_SOUTH
                         calibrationSets: [{
                           kind: DARK, explicitCount: 2,
                           gmosSouth: {
                             exposure: { seconds: 5 }
                             readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
                             dtax: ZERO
                             roi: FULL_FRAME
                           }
                         }]
                       }
                     }
                   }
                 }) { observation { id } }
               }
             """).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])
      _   <- expect(
        user     = staff,
        query    = s"""
          query {
            executionConfig(observationId: ${oid.asJson}) {
              gmosSouth {
                science {
                  nextAtom { description steps { observeClass stepConfig { stepType } instrumentConfig { exposure { seconds } } } }
                  possibleFuture { description }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "gmosSouth": {
                "science": {
                  "nextAtom": {
                    "description": "Dark x2",
                    "steps": [
                      { "observeClass": "DAY_CAL", "stepConfig": { "stepType": "DARK" }, "instrumentConfig": { "exposure": { "seconds": 5.000000 } } },
                      { "observeClass": "DAY_CAL", "stepConfig": { "stepType": "DARK" }, "instrumentConfig": { "exposure": { "seconds": 5.000000 } } }
                    ]
                  },
                  "possibleFuture": []
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("a visit on a calibration with no rows leaves them editable"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid)
      _   <- recordVisitAs(serviceUser, oid)
      _   <- query(staff, s"""
               mutation {
                 updateObservations(input: {
                   SET: { observingMode: { daytimeCalibration: { calibrationSets: [{ kind: DARK, explicitCount: 1, ${setup(1)} }] } } }
                   WHERE: { id: { EQ: ${oid.asJson} } }
                 }) { observations { id } }
               }
             """)
    yield ()

  test("count is capped"):
    for
      pid <- createProgramAs(staff)
      _   <- expectOdbError(
        user     = staff,
        query    = s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: { observingMode: { daytimeCalibration: { instrument: GMOS_NORTH, calibrationSets: [{ kind: DARK, explicitCount: 1001, ${setup(1)} }] } } }
            }) { observation { id } }
          }
        """,
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("at most 1000") => () }
      )
    yield ()

  test("an empty hand-entered sequence wins over the sets and leaves the observation Undefined"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- query(staff, s"""
               mutation {
                 replaceGmosNorthSequence(input: { observationId: ${oid.asJson}, sequenceType: SCIENCE, sequence: [] }) {
                   sequence { id }
                 }
               }
             """)
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""{ "executionConfig": { "gmosNorth": { "science": null } } }""".asRight
      )
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
                  "validationErrors": [ { "messages": [ "No sequence has been defined." ] } ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()
