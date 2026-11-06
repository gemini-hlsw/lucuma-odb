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
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Ghost
import lucuma.odb.smartgcal.data.SmartGcalValue
import skunk.Session

class daytimeCalibrationSets_ghost extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val staff       = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(staff, serviceUser)

  // Standard resolution at 1x1 binning, the fixed daytime static with the
  // setup below: one flat row (night baseline, two steps of 30 s red and 20 s
  // blue) and one arc row (day baseline, one step of 10 s red and 5 s blue).
  // The lookup's detector counts (3 red, 4 blue) differ from the setup's.
  private val smartKey: Ghost.SearchKey =
    Ghost.SearchKey(GhostResolutionMode.Standard, GhostBinning.OneByOne, GhostBinning.OneByOne)

  private def seconds(s: Long): TimeSpan =
    TimeSpan.unsafeFromMicroseconds(s * 1_000_000L)

  private def smartRow(gcal: Gcal, baseline: GcalBaselineType, count: Int, red: Long, blue: Long): Ghost.TableRow =
    Ghost.TableRow(
      smartKey,
      SmartGcalValue(gcal, baseline, PosInt.unsafeFrom(count), Ghost.GhostUpdate(seconds(red), PosInt.unsafeFrom(3), seconds(blue), PosInt.unsafeFrom(4)))
    )

  private val smartRows: List[Ghost.TableRow] =
    List(
      smartRow(Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen100W), GcalFilter.None, GcalDiffuser.Visible, GcalShutter.Open), GcalBaselineType.Night, 2, 30, 20),
      smartRow(Gcal(Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.ThArArc)), GcalFilter.None, GcalDiffuser.Visible, GcalShutter.Closed), GcalBaselineType.Day, 1, 10, 5)
    )

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    servicesFor(serviceUser).map(_(s)).use: services =>
      services.transactionally:
        smartRows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser(services.smartGcalService.insertGhost(i, PosLong.unsafeFrom(i + 1), r))

  private def setup(redSeconds: Int, blueSeconds: Int, binning: String = "ONE_BY_ONE"): String =
    s"""
      ghost: {
        red:  { exposureTime: { seconds: $redSeconds },  exposureCount: 1, binning: $binning, readMode: MEDIUM }
        blue: { exposureTime: { seconds: $blueSeconds }, exposureCount: 2, binning: $binning, readMode: SLOW }
        ifu1FiberAgitator: DISABLED
        ifu2FiberAgitator: DISABLED
      }
    """

  private val flatGcal: String =
    "gcal: { continuum: QUARTZ_HALOGEN100, filter: NONE, shutter: OPEN }"

  private val arcGcal: String =
    "gcal: { arcs: [TH_AR_ARC], filter: NONE, shutter: CLOSED }"

  private val mockupSets: String =
    List(
      s"{ kind: BIAS, explicitCount: 5, ${setup(0, 0)} }",
      s"{ kind: DARK, explicitCount: 10, ${setup(60, 40)} }",
      s"{ kind: FLAT, explicitCount: 3, $flatGcal, ${setup(30, 20)} }",
      s"{ kind: ARC, explicitCount: 2, $arcGcal, ${setup(10, 5)} }"
    ).mkString(", ")

  private def createQuery(pid: Program.Id, sets: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              daytimeCalibration: { instrument: GHOST, calibrationSets: [$sets] }
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
        instrumentConfig {
          red  { exposureTime { seconds } exposureCount }
          blue { exposureTime { seconds } exposureCount }
        }
      }
    """

  private def executionQuery(oid: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: ${oid.asJson}) {
          ghost {
            static { resolutionMode ifuMapping { mappingType } }
            science {
              nextAtom { $atomSelection }
              possibleFuture { $atomSelection }
              hasMore
            }
          }
        }
      }
    """

  private def step(stepType: String, redSeconds: Int, blueSeconds: Int, redCount: Int = 1, blueCount: Int = 2): Json =
    json"""
      {
        "observeClass": "DAY_CAL",
        "stepConfig": { "stepType": $stepType },
        "telescopeConfig": { "guiding": "DISABLED" },
        "instrumentConfig": {
          "red":  { "exposureTime": { "seconds": ${BigDecimal(redSeconds).setScale(6)} },  "exposureCount": $redCount },
          "blue": { "exposureTime": { "seconds": ${BigDecimal(blueSeconds).setScale(6)} }, "exposureCount": $blueCount }
        }
      }
    """

  private def atom(kind: String, count: Int, stepType: String, redSeconds: Int, blueSeconds: Int): Json =
    json"""{ "description": ${s"$kind x$count"}, "steps": ${List.fill(count)(step(stepType, redSeconds, blueSeconds))} }"""

  private val mockupAtoms: List[Json] =
    List(
      atom("Bias", 5,  "BIAS", 0,  0),
      atom("Dark", 10, "DARK", 60, 40),
      atom("Flat", 3,  "GCAL", 30, 20),
      atom("Arc",  2,  "GCAL", 10, 5)
    )

  private def expectedExecution(atoms: List[Json]): Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": { "resolutionMode": "STANDARD", "ifuMapping": { "mappingType": "NO_TARGET" } },
            "science": {
              "nextAtom": ${atoms.head},
              "possibleFuture": ${atoms.tail},
              "hasMore": false
            }
          }
        }
      }
    """

  test("one atom per set, count steps each, in row order, fixed static"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- expect(staff, executionQuery(oid), expectedExecution(mockupAtoms).asRight)
    yield ()

  test("sets round trip through the observing mode, Visible diffuser by default"):
    def ghost(red: Int, blue: Int): Json =
      json"""
        {
          "red":  { "exposureTime": { "seconds": ${BigDecimal(red).setScale(6)} },  "exposureCount": 1, "binning": "ONE_BY_ONE", "readMode": "MEDIUM" },
          "blue": { "exposureTime": { "seconds": ${BigDecimal(blue).setScale(6)} }, "exposureCount": 2, "binning": "ONE_BY_ONE", "readMode": "SLOW" },
          "ifu1FiberAgitator": "DISABLED",
          "ifu2FiberAgitator": "DISABLED",
          "centralWavelength": { "nanometers": 655.000 }
        }
      """
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
                    gnirs { exposure { seconds } }
                    ghost {
                      red  { exposureTime { seconds } exposureCount binning readMode }
                      blue { exposureTime { seconds } exposureCount binning readMode }
                      ifu1FiberAgitator
                      ifu2FiberAgitator
                      centralWavelength { nanometers }
                    }
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
                  "instrument": "GHOST",
                  "calibrationSets": [
                    { "kind": "BIAS", "count": 5,  "gcal": null, "gnirs": null, "ghost": ${ghost(0, 0)} },
                    { "kind": "DARK", "count": 10, "gcal": null, "gnirs": null, "ghost": ${ghost(60, 40)} },
                    {
                      "kind": "FLAT", "count": 3,
                      "gcal": { "continuum": "QUARTZ_HALOGEN100", "arcs": [], "filter": "NONE", "diffuser": "VISIBLE", "shutter": "OPEN" },
                      "gnirs": null, "ghost": ${ghost(30, 20)}
                    },
                    {
                      "kind": "ARC", "count": 2,
                      "gcal": { "continuum": null, "arcs": ["TH_AR_ARC"], "filter": "NONE", "diffuser": "VISIBLE", "shutter": "CLOSED" },
                      "gnirs": null, "ghost": ${ghost(10, 5)}
                    }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("a bias zeroes both detector exposure times"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: BIAS, explicitCount: 1, ${setup(7, 3)} }")
      res <- query(staff, s"""
        query {
          observation(observationId: ${oid.asJson}) {
            observingMode { daytimeCalibration { calibrationSets { ghost { red { exposureTime { seconds } exposureCount } blue { exposureTime { seconds } exposureCount } } } } }
          }
        }
      """)
    yield assertEquals(
      res.hcursor.downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets").focus.get,
      json"""
        [
          {
            "ghost": {
              "red":  { "exposureTime": { "seconds": 0.000000 }, "exposureCount": 1 },
              "blue": { "exposureTime": { "seconds": 0.000000 }, "exposureCount": 2 }
            }
          }
        ]
      """
    )

  test("a daytime observe is rejected"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: DAYTIME_OBSERVE, explicitCount: 1, ${setup(1, 1)} }", "must be one of BIAS, DARK, FLAT, ARC, SMART_FLAT, SMART_ARC, DAY_BASELINE, not DAYTIME_OBSERVE")
    yield ()

  test("the setup must be for the mode's instrument"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(
        pid,
        "{ kind: DARK, explicitCount: 1, igrins2: { exposure: { seconds: 1 } } }",
        "is for IGRINS-2 but"
      )
    yield ()

  test("defined with sets, four atoms in the digest"):
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
              "execution": { "digest": { "value": { "science": { "atomCount": 4, "observeClass": "DAY_CAL" } } } }
            }
          }
        """.asRight
      )
    yield ()

  test("no sets: empty sequence and Undefined"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid, Instrument.Ghost)
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""{ "executionConfig": { "ghost": { "static": { "resolutionMode": "STANDARD", "ifuMapping": { "mappingType": "NO_TARGET" } }, "science": null } } }""".asRight
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
                 replaceGhostSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", ghostDarkStep))}) {
                   sequence { id }
                 }
               }
             """)
      _   <- expect(
        staff,
        executionQuery(oid),
        expectedExecution(List(json"""{ "description": "Darks", "steps": [ ${step("DARK", 10, 10, 1, 1)} ] }""")).asRight
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

  test("smart kinds are accepted on GHOST, storing no count, zero exposure times and single exposures"):
    val detector = json"""{ "exposureTime": { "seconds": 0.000000 }, "exposureCount": 1 }"""
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup(1, 1)} }, { kind: SMART_ARC, ${setup(1, 1)} }, { kind: DAY_BASELINE, ${setup(1, 1)} }")
      res <- query(staff, s"""
        query {
          observation(observationId: ${oid.asJson}) {
            observingMode { daytimeCalibration { calibrationSets { kind explicitCount gcal { continuum } ghost { red { exposureTime { seconds } exposureCount } blue { exposureTime { seconds } exposureCount } } } } }
          }
        }
      """)
    yield assertEquals(
      res.hcursor.downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets").focus.get,
      json"""
        [
          { "kind": "SMART_FLAT",   "explicitCount": null, "gcal": null, "ghost": { "red": $detector, "blue": $detector } },
          { "kind": "SMART_ARC",    "explicitCount": null, "gcal": null, "ghost": { "red": $detector, "blue": $detector } },
          { "kind": "DAY_BASELINE", "explicitCount": null, "gcal": null, "ghost": { "red": $detector, "blue": $detector } }
        ]
      """
    )

  private val smartAtomSelection: String =
    """
      description
      steps {
        observeClass
        stepConfig { stepType ... on Gcal { continuum arcs } }
        instrumentConfig {
          red  { exposureTime { seconds } exposureCount }
          blue { exposureTime { seconds } exposureCount }
        }
      }
    """

  // The lookup supplies both detectors' exposure time and count.
  private def smartGcalStep(continuum: Option[String], arcs: List[String], red: Int, blue: Int): Json =
    json"""
      {
        "observeClass": "DAY_CAL",
        "stepConfig": { "stepType": "GCAL", "continuum": $continuum, "arcs": $arcs },
        "instrumentConfig": {
          "red":  { "exposureTime": { "seconds": ${BigDecimal(red).setScale(6)} },  "exposureCount": 3 },
          "blue": { "exposureTime": { "seconds": ${BigDecimal(blue).setScale(6)} }, "exposureCount": 4 }
        }
      }
    """

  private val smartFlatStep: Json = smartGcalStep(Some("QUARTZ_HALOGEN100"), Nil, 30, 20)
  private val smartArcStep: Json  = smartGcalStep(None, List("TH_AR_ARC"), 10, 5)

  test("smart rows expand through the GHOST Smart GCAL tables, counts included"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup(1, 1)} }, { kind: SMART_ARC, ${setup(1, 1)} }, { kind: DAY_BASELINE, ${setup(1, 1)} }")
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
              ghost {
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
              "ghost": {
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

  // The seeded rows are for 1x1 binning only.
  test("a smart flat with an unmapped binning fails generation, naming the row"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${setup(1, 1)} }, { kind: SMART_FLAT, ${setup(1, 1, "TWO_BY_TWO")} }")
      _   <- expectOdbError(
        user     = staff,
        query    = executionQuery(oid),
        expected = { case OdbError.SequenceUnavailable(_, Some(m)) if m.startsWith("Calibration set 2 (Smart Flat): missing Smart GCAL mapping") => () }
      )
    yield ()
