// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.foldable.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Igrins2
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import skunk.Session

class daytimeCalibrationSets_igrins2 extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val staff       = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(staff, serviceUser)

  // IGRINS-2 has no configurable items, so one flat row (night baseline, two
  // 30 s steps) and one arc row (day baseline, one 10 s step) serve every setup.
  private def smartRow(gcal: Gcal, baseline: GcalBaselineType, count: Int, seconds: Long): Igrins2.TableRow =
    Igrins2.TableRow(
      PosLong.unsafeFrom(1),
      Igrins2.TableKey,
      SmartGcalValue(gcal, baseline, PosInt.unsafeFrom(count), LegacyInstrumentConfig(TimeSpan.unsafeFromMicroseconds(seconds * 1_000_000L)))
    )

  private val smartRows: List[Igrins2.TableRow] =
    List(
      smartRow(Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W), GcalFilter.Nd20, GcalDiffuser.Ir, GcalShutter.Closed), GcalBaselineType.Night, 2, 30),
      smartRow(Gcal(Gcal.Lamp.fromArcs(NonEmptySet.of(GcalArc.CuArArc, GcalArc.XeArc)), GcalFilter.None, GcalDiffuser.Ir, GcalShutter.Closed), GcalBaselineType.Day, 1, 10)
    )

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    servicesFor(serviceUser).map(_(s)).use: services =>
      services.transactionally:
        smartRows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser(services.smartGcalService.insertIgrins2(i, r))

  private def setup(exposureSeconds: Int): String =
    s"igrins2: { exposure: { seconds: $exposureSeconds } }"

  private val flatGcal: String =
    "gcal: { continuum: QUARTZ_HALOGEN5, filter: ND20, shutter: CLOSED }"

  private val arcGcal: String =
    "gcal: { arcs: [CU_AR_ARC, XE_ARC], filter: NONE, shutter: CLOSED }"

  private val mockupSets: String =
    List(
      s"{ kind: DARK, explicitCount: 10, ${setup(60)} }",
      s"{ kind: FLAT, explicitCount: 3, $flatGcal, ${setup(30)} }",
      s"{ kind: ARC, explicitCount: 2, $arcGcal, ${setup(10)} }"
    ).mkString(", ")

  private def createQuery(pid: Program.Id, sets: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              daytimeCalibration: { instrument: IGRINS2, calibrationSets: [$sets] }
            }
          }
        }) {
          observation { id }
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
        stepConfig { stepType }
        telescopeConfig { guiding }
        instrumentConfig { exposure { seconds } }
      }
    """

  private def executionQuery(oid: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: ${oid.asJson}) {
          igrins2 {
            science {
              nextAtom { $atomSelection }
              possibleFuture { $atomSelection }
              hasMore
            }
          }
        }
      }
    """

  private def atom(kind: String, count: Int, stepType: String, seconds: Int): Json =
    val step = json"""
      {
        "observeClass": "DAY_CAL",
        "stepConfig": { "stepType": $stepType },
        "telescopeConfig": { "guiding": "DISABLED" },
        "instrumentConfig": { "exposure": { "seconds": ${BigDecimal(seconds).setScale(6)} } }
      }
    """
    json"""{ "description": ${s"$kind x$count"}, "steps": ${List.fill(count)(step)} }"""

  private val mockupAtoms: List[Json] =
    List(
      atom("Dark", 10, "DARK", 60),
      atom("Flat", 3,  "GCAL", 30),
      atom("Arc",  2,  "GCAL", 10)
    )

  private def expectedExecution(atoms: List[Json]): Json =
    json"""
      {
        "executionConfig": {
          "igrins2": {
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

  test("sets round trip through the observing mode, IR diffuser by default"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- expect(
        user     = staff,
        query    = s"""
          query {
            observation(observationId: ${oid.asJson}) {
              observingMode {
                daytimeCalibration {
                  instrument
                  calibrationSets {
                    kind
                    count
                    gcal { continuum arcs filter diffuser shutter }
                    flamingos2 { filter }
                    igrins2 { exposure { seconds } centralWavelength { micrometers } }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "observingMode": {
                "daytimeCalibration": {
                  "instrument": "IGRINS2",
                  "calibrationSets": [
                    { "kind": "DARK", "count": 10, "gcal": null, "flamingos2": null, "igrins2": { "exposure": { "seconds": 60.000000 }, "centralWavelength": { "micrometers": 2.100000 } } },
                    { "kind": "FLAT", "count": 3,  "gcal": { "continuum": "QUARTZ_HALOGEN5", "arcs": [], "filter": "ND20", "diffuser": "IR", "shutter": "CLOSED" }, "flamingos2": null, "igrins2": { "exposure": { "seconds": 30.000000 }, "centralWavelength": { "micrometers": 2.100000 } } },
                    { "kind": "ARC",  "count": 2,  "gcal": { "continuum": null, "arcs": ["CU_AR_ARC", "XE_ARC"], "filter": "NONE", "diffuser": "IR", "shutter": "CLOSED" }, "flamingos2": null, "igrins2": { "exposure": { "seconds": 10.000000 }, "centralWavelength": { "micrometers": 2.100000 } } }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("a bias or daytime observe is rejected"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: BIAS, explicitCount: 1, ${setup(0)} }", "must be one of DARK, FLAT, ARC, SMART_FLAT, SMART_ARC, DAY_BASELINE, not BIAS")
      _   <- expectRejected(pid, s"{ kind: DAYTIME_OBSERVE, explicitCount: 1, ${setup(1)} }", "must be one of DARK, FLAT, ARC, SMART_FLAT, SMART_ARC, DAY_BASELINE, not DAYTIME_OBSERVE")
    yield ()

  test("the setup must be for the mode's instrument"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(
        pid,
        "{ kind: DARK, explicitCount: 1, flamingos2: { exposure: { seconds: 1 }, filter: J, readMode: BRIGHT, lyotWheel: F16, decker: LONG_SLIT, readoutMode: SCIENCE, reads: READS_1 } }",
        "is for Flamingos 2 but"
      )
    yield ()

  test("defined with sets, three atoms in the digest"):
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
              "execution": { "digest": { "value": { "science": { "atomCount": 3, "observeClass": "DAY_CAL" } } } }
            }
          }
        """.asRight
      )
    yield ()

  test("no sets: empty sequence and Undefined"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid, Instrument.Igrins2)
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""{ "executionConfig": { "igrins2": { "science": null } } }""".asRight
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

  test("a hand-entered sequence takes precedence over the sets"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- query(staff, s"""
               mutation {
                 replaceIgrins2Sequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", igrins2DarkStep))}) {
                   sequence { id }
                 }
               }
             """)
      _   <- expect(
        staff,
        executionQuery(oid),
        expectedExecution(List(json"""
          {
            "description": "Darks",
            "steps": [
              {
                "observeClass": "DAY_CAL",
                "stepConfig": { "stepType": "DARK" },
                "telescopeConfig": { "guiding": "DISABLED" },
                "instrumentConfig": { "exposure": { "seconds": 20.000000 } }
              }
            ]
          }
        """)).asRight
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

  test("rows are cloned with the observation"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      cid <- cloneObservationAs(staff, oid)
      _   <- expect(staff, executionQuery(cid), expectedExecution(mockupAtoms).asRight)
    yield ()

  test("smart kinds are accepted on IGRINS-2"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup(1)} }, { kind: SMART_ARC, ${setup(1)} }, { kind: DAY_BASELINE, ${setup(1)} }")
      res <- query(staff, s"""
        query {
          observation(observationId: ${oid.asJson}) {
            observingMode { daytimeCalibration { calibrationSets { kind explicitCount gcal { continuum } igrins2 { exposure { seconds } } } } }
          }
        }
      """)
    yield assertEquals(
      res.hcursor.downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets").focus.get,
      json"""
        [
          { "kind": "SMART_FLAT",   "explicitCount": null, "gcal": null, "igrins2": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "SMART_ARC",    "explicitCount": null, "gcal": null, "igrins2": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "DAY_BASELINE", "explicitCount": null, "gcal": null, "igrins2": { "exposure": { "seconds": 0.000000 } } }
        ]
      """
    )

  private val smartAtomSelection: String =
    """
      description
      steps {
        observeClass
        stepConfig { stepType ... on Gcal { continuum arcs } }
        instrumentConfig { exposure { seconds } }
      }
    """

  private def smartGcalStep(continuum: Option[String], arcs: List[String], seconds: Int): Json =
    json"""
      {
        "observeClass": "DAY_CAL",
        "stepConfig": { "stepType": "GCAL", "continuum": $continuum, "arcs": $arcs },
        "instrumentConfig": { "exposure": { "seconds": ${BigDecimal(seconds).setScale(6)} } }
      }
    """

  private val smartFlatStep: Json = smartGcalStep(Some("QUARTZ_HALOGEN5"), Nil, 30)
  private val smartArcStep: Json  = smartGcalStep(None, List("CU_AR_ARC", "XE_ARC"), 10)

  test("smart rows expand through the IGRINS-2 Smart GCAL tables, counts included"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup(1)} }, { kind: SMART_ARC, ${setup(1)} }, { kind: DAY_BASELINE, ${setup(1)} }")
      _   <- expect(
        user     = staff,
        query    = s"""
          query {
            observation(observationId: ${oid.asJson}) {
              observingMode { daytimeCalibration { calibrationSets { kind count } } }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "observingMode": {
                "daytimeCalibration": {
                  "calibrationSets": [
                    { "kind": "SMART_FLAT",   "count": 2 },
                    { "kind": "SMART_ARC",    "count": 1 },
                    { "kind": "DAY_BASELINE", "count": 1 }
                  ]
                }
              }
            }
          }
        """.asRight
      )
      _   <- expect(
        user     = staff,
        query    = s"""
          query {
            executionConfig(observationId: ${oid.asJson}) {
              igrins2 {
                science {
                  nextAtom { $smartAtomSelection }
                  possibleFuture { $smartAtomSelection }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "igrins2": {
                "science": {
                  "nextAtom": { "description": "Smart Flat", "steps": [ $smartFlatStep, $smartFlatStep ] },
                  "possibleFuture": [
                    { "description": "Smart Arc",    "steps": [ $smartArcStep ] },
                    { "description": "Day Baseline", "steps": [ $smartArcStep ] }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()
