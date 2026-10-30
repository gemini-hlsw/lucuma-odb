// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

/**
 * Smart calibration sets resolve through the Smart GCAL tables seeded by
 * ExecutionTestSupportForGmos: the R831 + 1.00" slit key has a night flat
 * (QUARTZ_HALOGEN5, 1 s) and a day arc (CUAR, 1 s), one step each.
 */
class daytimeCalibrationSets_smart extends ExecutionTestSupportForGmos with DaytimeCalibrationOps:

  // Matches gn_key_1_00; `mapped = false` picks a grating with no table row.
  private def setup(mapped: Boolean = true, exposureSeconds: Int = 7): String =
    val grating = if mapped then "R831_G5302" else "B1200_G5301"
    s"""
      gmosNorth: {
        exposure: { seconds: $exposureSeconds }
        readout: { xBin: TWO, yBin: TWO, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
        dtax: ZERO
        roi: FULL_FRAME
        gratingConfig: { grating: $grating, order: ONE, wavelength: { nanometers: 615 } }
        fpu: { builtin: LONG_SLIT_1_00 }
      }
    """

  private def createQuery(pid: Program.Id, sets: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              daytimeCalibration: { instrument: GMOS_NORTH, calibrationSets: [$sets] }
            }
          }
        }) {
          observation {
            id
            observingMode {
              daytimeCalibration {
                calibrationSets {
                  kind
                  count
                  gcal { continuum arcs }
                  gmosNorth { exposure { seconds } }
                }
              }
            }
          }
        }
      }
    """

  private def createWithSets(pid: Program.Id, sets: String): IO[Observation.Id] =
    query(staff, createQuery(pid, sets))
      .map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  private def expectRejected(pid: Program.Id, sets: String, fragment: String): IO[Unit] =
    expectOdbError(
      user     = staff,
      query    = createQuery(pid, sets),
      expected = { case OdbError.InvalidArgument(Some(m)) if m.contains(fragment) => () }
    )

  private val atomSelection: String =
    """
      description
      steps {
        observeClass
        stepConfig { stepType ... on Gcal { continuum arcs } }
        instrumentConfig { exposure { seconds } }
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
            }
          }
        }
      }
    """

  private def gcalStep(continuum: Option[String], arcs: List[String]): Json =
    json"""
      {
        "observeClass": "DAY_CAL",
        "stepConfig": { "stepType": "GCAL", "continuum": $continuum, "arcs": $arcs },
        "instrumentConfig": { "exposure": { "seconds": 1.000000 } }
      }
    """

  private def atom(description: String, steps: List[Json]): Json =
    json"""{ "description": $description, "steps": $steps }"""

  private val flatStep = gcalStep("QUARTZ_HALOGEN5".some, Nil)
  private val arcStep  = gcalStep(None, List("CU_AR_ARC"))

  test("a smart set takes no count"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: SMART_FLAT, count: 2, ${setup()} }", "takes no `count`")
    yield ()

  test("a smart set takes no gcal"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: SMART_ARC, gcal: { arcs: [AR_ARC], filter: ND10, shutter: CLOSED }, ${setup()} }", "takes no `gcal`")
    yield ()

  test("a manual set still needs a count"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: DARK, ${setup()} }", "needs a `count`")
    yield ()

  test("a row read back round-trips into an update unchanged"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup()} }, { kind: DARK, count: 4, ${setup()} }")
      _   <- expect(
        user     = staff,
        query    = s"""
          mutation {
            updateObservations(input: {
              SET: {
                observingMode: {
                  daytimeCalibration: {
                    instrument: GMOS_NORTH
                    calibrationSets: [
                      { kind: SMART_FLAT, count: null, gcal: null, ${setup(exposureSeconds = 0)} },
                      { kind: DARK, count: 4, gcal: null, ${setup()} }
                    ]
                  }
                }
              }
              WHERE: { id: { EQ: ${oid.asJson} } }
            }) {
              observations {
                observingMode { daytimeCalibration { calibrationSets { kind count stepCount } } }
              }
            }
          }
        """,
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "observingMode": {
                    "daytimeCalibration": {
                      "calibrationSets": [
                        { "kind": "SMART_FLAT", "count": null, "stepCount": 1 },
                        { "kind": "DARK",       "count": 4,    "stepCount": 4 }
                      ]
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("smart rows store no count or gcal and a zero exposure; stepCount is the lookup's"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup()} }, { kind: DAY_BASELINE, ${setup()} }, { kind: SMART_ARC, ${setup(mapped = false)} }, { kind: DARK, count: 4, ${setup()} }")
      res <- query(staff, s"""
        query {
          observation(observationId: ${oid.asJson}) {
            observingMode {
              daytimeCalibration {
                calibrationSets { kind count stepCount gcal { continuum } gmosNorth { exposure { seconds } } }
              }
            }
          }
        }
      """)
    yield assertEquals(
      res.hcursor.downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets").focus.get,
      json"""
        [
          { "kind": "SMART_FLAT",   "count": null, "stepCount": 1,    "gcal": null, "gmosNorth": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "DAY_BASELINE", "count": null, "stepCount": 1,    "gcal": null, "gmosNorth": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "SMART_ARC",    "count": null, "stepCount": null, "gcal": null, "gmosNorth": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "DARK",         "count": 4,    "stepCount": 4,    "gcal": null, "gmosNorth": { "exposure": { "seconds": 7.000000 } } }
        ]
      """
    )

  test("smart rows expand to the lookup's steps, one atom each, all DAY_CAL"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup()} }, { kind: SMART_ARC, ${setup()} }, { kind: DAY_BASELINE, ${setup()} }")
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""
          {
            "executionConfig": {
              "gmosNorth": {
                "science": {
                  "nextAtom": ${atom("Smart Flat", List(flatStep))},
                  "possibleFuture": [
                    ${atom("Smart Arc", List(arcStep))},
                    ${atom("Day Baseline", List(arcStep))}
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  // A generation failure is reported by the execution config and the digest;
  // the workflow does not look at it, as for every other mode.
  test("a smart row with no mapping fails generation, naming the row"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup()} }, { kind: SMART_ARC, ${setup(mapped = false)} }")
      _   <- expectOdbError(
        user     = staff,
        query    = executionQuery(oid),
        expected = { case OdbError.SequenceUnavailable(_, Some(m)) if m.startsWith("Calibration set 1 (SMART_ARC) could not be resolved: missing Smart GCAL mapping") => () }
      )
      _   <- runObscalcUpdate(pid, oid)
      res <- query(staff, workflowQuery(oid))
    yield assertEquals(
      res.hcursor.downFields("observation", "workflow", "value", "state").require[String],
      "DEFINED"
    )
