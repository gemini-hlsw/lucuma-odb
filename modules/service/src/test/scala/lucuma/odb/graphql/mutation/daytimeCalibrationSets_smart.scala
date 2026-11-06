// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import clue.ResponseException
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorClueExtensions.fromGraphQLError
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos
import skunk.syntax.all.*

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

  // One InvalidArgument per unmapped row, in row order.
  private def expectUnmapped(query: String, rows: List[(Int, String)]): IO[Unit] =
    this.query(staff, query).intercept[ResponseException[Any]].map: e =>
      val ms = e.errors.toList.flatMap(OdbError.fromGraphQLError(_).toList).collect:
        case OdbError.InvalidArgument(Some(m)) => m
      assertEquals(ms.length, rows.length, ms.mkString("\n"))
      rows.zip(ms).foreach: (row, m) =>
        val (n, kind) = row
        assert(m.startsWith(s"Calibration set $n ($kind): missing Smart GCAL mapping: GmosNorth"), m)

  private val b1200Key: Gmos.TableKey.North =
    gn_key_1_00.copy(gratingConfig = gn_key_1_00.gratingConfig.map(_.copy(grating = GmosNorthGrating.B1200_G5301)))

  // The unmapped setup gains rows, so its sets can be written, and loses them
  // again to model a table reload.
  private def seedB1200: IO[Unit] =
    withSession: s =>
      servicesFor(staff).map(_(s)).use: services =>
        services.transactionally:
          Services.asSuperUser:
            services.smartGcalService.insertGmosNorth(901, Gmos.TableRow(PosLong.unsafeFrom(1), b1200Key, gmos_flat)) *>
              services.smartGcalService.insertGmosNorth(902, Gmos.TableRow(PosLong.unsafeFrom(1), b1200Key, gmos_arc))

  private def withdrawB1200: IO[Unit] =
    withSession(_.execute(sql"DELETE FROM t_smart_gmos_north WHERE c_disperser = 'B1200_G5301'".command).void)

  private def setsQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: ${oid.asJson}) {
          observingMode {
            daytimeCalibration {
              calibrationSets { kind explicitCount count mappingError gcal { continuum } gmosNorth { exposure { seconds } } }
            }
          }
        }
      }
    """

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

  test("a smart set takes no explicit count"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: SMART_FLAT, explicitCount: 2, ${setup()} }", "takes no `explicitCount`")
    yield ()

  test("a smart set takes no gcal"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: SMART_ARC, gcal: { arcs: [AR_ARC], filter: ND10, shutter: CLOSED }, ${setup()} }", "takes no `gcal`")
    yield ()

  test("a manual set still needs an explicit count"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: DARK, ${setup()} }", "needs an `explicitCount`")
    yield ()

  test("a row read back round-trips into an update unchanged"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup()} }, { kind: DARK, explicitCount: 4, ${setup()} }")
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
                      { kind: SMART_FLAT, explicitCount: null, gcal: null, ${setup(exposureSeconds = 0)} },
                      { kind: DARK, explicitCount: 4, gcal: null, ${setup()} }
                    ]
                  }
                }
              }
              WHERE: { id: { EQ: ${oid.asJson} } }
            }) {
              observations {
                observingMode { daytimeCalibration { calibrationSets { kind explicitCount count } } }
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
                        { "kind": "SMART_FLAT", "explicitCount": null, "count": 1 },
                        { "kind": "DARK",       "explicitCount": 4,    "count": 4 }
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

  test("smart rows store no count or gcal and a zero exposure; count is the lookup's"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup()} }, { kind: DAY_BASELINE, ${setup()} }, { kind: SMART_ARC, ${setup()} }, { kind: DARK, explicitCount: 4, ${setup()} }")
      res <- query(staff, setsQuery(oid))
    yield assertEquals(
      res.hcursor.downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets").focus.get,
      json"""
        [
          { "kind": "SMART_FLAT",   "explicitCount": null, "count": 1, "mappingError": null, "gcal": null, "gmosNorth": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "DAY_BASELINE", "explicitCount": null, "count": 1, "mappingError": null, "gcal": null, "gmosNorth": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "SMART_ARC",    "explicitCount": null, "count": 1, "mappingError": null, "gcal": null, "gmosNorth": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "DARK",         "explicitCount": 4,    "count": 4, "mappingError": null, "gcal": null, "gmosNorth": { "exposure": { "seconds": 7.000000 } } }
        ]
      """
    )

  test("an unmapped smart row is refused on create, naming each one"):
    for
      pid <- createProgramAs(staff)
      _   <- expectUnmapped(
        createQuery(pid, s"{ kind: SMART_FLAT, ${setup(mapped = false)} }, { kind: DARK, explicitCount: 2, ${setup(mapped = false)} }, { kind: SMART_ARC, ${setup(mapped = false)} }"),
        List(1 -> "Smart Flat", 3 -> "Smart Arc")
      )
    yield ()

  test("an unmapped smart row is refused on update and nothing changes"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup()} }")
      _   <- expectUnmapped(
        s"""
          mutation {
            updateObservations(input: {
              SET: { observingMode: { daytimeCalibration: { calibrationSets: [{ kind: SMART_ARC, ${setup(mapped = false)} }] } } }
              WHERE: { id: { EQ: ${oid.asJson} } }
            }) { observations { id } }
          }
        """,
        List(1 -> "Smart Arc")
      )
      res <- query(staff, setsQuery(oid))
    yield assertEquals(
      res.hcursor.downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets").downArray.downField("kind").require[String],
      "SMART_FLAT"
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

  // A row written while mapped may lose its table row later.  Generation then
  // fails naming the row, the row reports why, and the workflow does not look
  // at it, as for every other mode.  A clone copies the row as it is.
  test("a smart row that loses its mapping fails generation, naming the row"):
    for
      pid <- createProgramAs(staff)
      _   <- seedB1200
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup()} }, { kind: SMART_ARC, ${setup(mapped = false)} }")
      _   <- withdrawB1200
      res <- query(staff, setsQuery(oid))
      _   <- cloneObservationAs(staff, oid)
      _   <- expectOdbError(
        user     = staff,
        query    = executionQuery(oid),
        expected = { case OdbError.SequenceUnavailable(_, Some(m)) if m.startsWith("Calibration set 2 (Smart Arc): missing Smart GCAL mapping") => () }
      )
      _   <- runObscalcUpdate(pid, oid)
      wf  <- query(staff, workflowQuery(oid))
    yield
      val rows = res.hcursor.downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets").downArray
      assertEquals(rows.downField("mappingError").require[Option[String]], none)
      assert(rows.right.downField("mappingError").require[String].startsWith("missing Smart GCAL mapping: GmosNorth"))
      assertEquals(wf.hcursor.downFields("observation", "workflow", "value", "state").require[String], "DEFINED")
