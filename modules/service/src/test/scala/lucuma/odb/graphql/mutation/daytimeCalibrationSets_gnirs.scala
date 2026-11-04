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
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gnirs
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import skunk.Session

class daytimeCalibrationSets_gnirs extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val staff       = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(staff, serviceUser)

  // The long slit setup below, 32 l/mm at 2.2 µm through the 0.30" slit on the
  // short camera, has one flat row (night baseline, two 30 s steps) and one arc
  // row (day baseline, one 10 s step).  Shallow is the fixed daytime well depth.
  private val smartKey: Gnirs.TableKey =
    Gnirs.TableKey(
      GnirsPixelScale.PixelScale_0_15,
      GnirsGrating.D32,
      GnirsPrism.Mirror,
      BoundedInterval.unsafeOpenUpper(
        Wavelength.fromIntNanometers(900).get,
        Wavelength.fromIntNanometers(2560).get
      ),
      GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
      GnirsWellDepth.Shallow
    )

  private def smartRow(gcal: Gcal, baseline: GcalBaselineType, count: Int, seconds: Long): Gnirs.TableRow =
    Gnirs.TableRow(
      PosLong.unsafeFrom(1),
      smartKey,
      SmartGcalValue(gcal, baseline, PosInt.unsafeFrom(count), LegacyInstrumentConfig(TimeSpan.unsafeFromMicroseconds(seconds * 1_000_000L), PosInt.unsafeFrom(1)))
    )

  private val smartRows: List[Gnirs.TableRow] =
    List(
      smartRow(Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.IrGreyBodyHigh), GcalFilter.Nir, GcalDiffuser.Ir, GcalShutter.Open), GcalBaselineType.Night, 2, 30),
      smartRow(Gcal(Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.ArArc)), GcalFilter.None, GcalDiffuser.Ir, GcalShutter.Closed), GcalBaselineType.Day, 1, 10)
    )

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    servicesFor(serviceUser).map(_(s)).use: services =>
      services.transactionally:
        smartRows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser(services.smartGcalService.insertGnirs(i, r))

  // Long slit spectroscopy: the acquisition mirror is out of the beam.
  private def spectroscopy(exposureSeconds: Int): String =
    s"""
      gnirs: {
        exposure: { seconds: $exposureSeconds }
        coadds: 1
        filter: ORDER3
        decker: SHORT_CAM_LONG_SLIT
        fpuSlit: LONG_SLIT_0_30
        acquisitionMirrorOut: { prism: MIRROR, grating: D32, wavelength: { micrometers: 2.2 } }
        camera: SHORT_BLUE
        readMode: BRIGHT
      }
    """

  // Imaging through the pupil viewer: the acquisition mirror is in the beam.
  private def imaging(exposureSeconds: Int): String =
    s"""
      gnirs: {
        exposure: { seconds: $exposureSeconds }
        coadds: 1
        filter: ORDER4
        decker: PUPIL_VIEWER
        fpuOther: PUPIL_VIEWER
        camera: SHORT_BLUE
        readMode: BRIGHT
      }
    """

  private val flatGcal: String =
    "gcal: { continuum: IR_GREY_BODY_HIGH, filter: NIR, shutter: OPEN }"

  private val arcGcal: String =
    "gcal: { arcs: [AR_ARC], filter: NONE, shutter: CLOSED }"

  private val mockupSets: String =
    List(
      s"{ kind: DARK, explicitCount: 10, ${spectroscopy(60)} }",
      s"{ kind: FLAT, explicitCount: 3, $flatGcal, ${spectroscopy(30)} }",
      s"{ kind: ARC, explicitCount: 2, $arcGcal, ${spectroscopy(10)} }",
      s"{ kind: DAYTIME_OBSERVE, explicitCount: 1, ${imaging(5)} }"
    ).mkString(", ")

  private def createQuery(pid: Program.Id, sets: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              daytimeCalibration: { instrument: GNIRS, calibrationSets: [$sets] }
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
          gnirs {
            static { wellDepth }
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
      atom("Dark",    10, "DARK",    60),
      atom("Flat",    3,  "GCAL",    30),
      atom("Arc",     2,  "GCAL",    10),
      atom("Observe", 1,  "SCIENCE", 5)
    )

  private def expectedExecution(atoms: List[Json]): Json =
    json"""
      {
        "executionConfig": {
          "gnirs": {
            "static": { "wellDepth": "SHALLOW" },
            "science": {
              "nextAtom": ${atoms.head},
              "possibleFuture": ${atoms.tail},
              "hasMore": false
            }
          }
        }
      }
    """

  test("one atom per set, count steps each, in row order, Shallow well depth"):
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
                    igrins2 { exposure { seconds } }
                    gnirs {
                      exposure { seconds }
                      coadds
                      filter
                      decker
                      fpuSlit
                      fpuOther
                      fpuIfu
                      acquisitionMirrorOut { prism grating wavelength { micrometers } }
                      centralWavelength { micrometers }
                      camera
                      focusMotorSteps
                      readMode
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
                  "instrument": "GNIRS",
                  "calibrationSets": [
                    {
                      "kind": "DARK", "count": 10, "gcal": null, "igrins2": null,
                      "gnirs": {
                        "exposure": { "seconds": 60.000000 }, "coadds": 1, "filter": "ORDER3", "decker": "SHORT_CAM_LONG_SLIT",
                        "fpuSlit": "LONG_SLIT_0_30", "fpuOther": null, "fpuIfu": null,
                        "acquisitionMirrorOut": { "prism": "MIRROR", "grating": "D32", "wavelength": { "micrometers": 2.200000 } },
                        "centralWavelength": { "micrometers": 2.200000 }, "camera": "SHORT_BLUE", "focusMotorSteps": null, "readMode": "BRIGHT"
                      }
                    },
                    {
                      "kind": "FLAT", "count": 3,
                      "gcal": { "continuum": "IR_GREY_BODY_HIGH", "arcs": [], "filter": "NIR", "diffuser": "IR", "shutter": "OPEN" },
                      "igrins2": null,
                      "gnirs": {
                        "exposure": { "seconds": 30.000000 }, "coadds": 1, "filter": "ORDER3", "decker": "SHORT_CAM_LONG_SLIT",
                        "fpuSlit": "LONG_SLIT_0_30", "fpuOther": null, "fpuIfu": null,
                        "acquisitionMirrorOut": { "prism": "MIRROR", "grating": "D32", "wavelength": { "micrometers": 2.200000 } },
                        "centralWavelength": { "micrometers": 2.200000 }, "camera": "SHORT_BLUE", "focusMotorSteps": null, "readMode": "BRIGHT"
                      }
                    },
                    {
                      "kind": "ARC", "count": 2,
                      "gcal": { "continuum": null, "arcs": ["AR_ARC"], "filter": "NONE", "diffuser": "IR", "shutter": "CLOSED" },
                      "igrins2": null,
                      "gnirs": {
                        "exposure": { "seconds": 10.000000 }, "coadds": 1, "filter": "ORDER3", "decker": "SHORT_CAM_LONG_SLIT",
                        "fpuSlit": "LONG_SLIT_0_30", "fpuOther": null, "fpuIfu": null,
                        "acquisitionMirrorOut": { "prism": "MIRROR", "grating": "D32", "wavelength": { "micrometers": 2.200000 } },
                        "centralWavelength": { "micrometers": 2.200000 }, "camera": "SHORT_BLUE", "focusMotorSteps": null, "readMode": "BRIGHT"
                      }
                    },
                    {
                      "kind": "DAYTIME_OBSERVE", "count": 1, "gcal": null, "igrins2": null,
                      "gnirs": {
                        "exposure": { "seconds": 5.000000 }, "coadds": 1, "filter": "ORDER4", "decker": "PUPIL_VIEWER",
                        "fpuSlit": null, "fpuOther": "PUPIL_VIEWER", "fpuIfu": null,
                        "acquisitionMirrorOut": null,
                        "centralWavelength": { "micrometers": 1.650000 }, "camera": "SHORT_BLUE", "focusMotorSteps": null, "readMode": "BRIGHT"
                      }
                    }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("a bias is rejected"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: BIAS, explicitCount: 1, ${spectroscopy(0)} }", "must be one of DARK, FLAT, ARC, DAYTIME_OBSERVE, SMART_FLAT, SMART_ARC, DAY_BASELINE, not BIAS")
    yield ()

  test("a smart arc needs the acquisition mirror out; a manual arc does not"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: SMART_ARC, ${imaging(1)} }", "A SMART_ARC calibration set needs a disperser.")
      _   <- createWithSets(pid, s"{ kind: ARC, explicitCount: 1, $arcGcal, ${imaging(1)} }")
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
      oid <- createDaytimeCalibrationAs(staff, pid, Instrument.Gnirs)
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""{ "executionConfig": { "gnirs": { "static": { "wellDepth": "SHALLOW" }, "science": null } } }""".asRight
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
                 replaceGnirsSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", gnirsDarkStep))}) {
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

  test("smart kinds are accepted on GNIRS, storing no count and a zero exposure"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${spectroscopy(1)} }, { kind: SMART_ARC, ${spectroscopy(1)} }, { kind: DAY_BASELINE, ${spectroscopy(1)} }")
      res <- query(staff, s"""
        query {
          observation(observationId: ${oid.asJson}) {
            observingMode { daytimeCalibration { calibrationSets { kind explicitCount gcal { continuum } gnirs { exposure { seconds } } } } }
          }
        }
      """)
    yield assertEquals(
      res.hcursor.downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets").focus.get,
      json"""
        [
          { "kind": "SMART_FLAT",   "explicitCount": null, "gcal": null, "gnirs": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "SMART_ARC",    "explicitCount": null, "gcal": null, "gnirs": { "exposure": { "seconds": 0.000000 } } },
          { "kind": "DAY_BASELINE", "explicitCount": null, "gcal": null, "gnirs": { "exposure": { "seconds": 0.000000 } } }
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

  private val smartFlatStep: Json = smartGcalStep(Some("IR_GREY_BODY_HIGH"), Nil, 30)
  private val smartArcStep: Json  = smartGcalStep(None, List("AR_ARC"), 10)

  test("smart rows expand through the GNIRS Smart GCAL tables, counts included"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${spectroscopy(1)} }, { kind: SMART_ARC, ${spectroscopy(1)} }, { kind: DAY_BASELINE, ${spectroscopy(1)} }")
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
              gnirs {
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
              "gnirs": {
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

  // The GNIRS Smart GCAL key has no grating when the acquisition mirror is in,
  // so a smart flat on an imaging setup never resolves.
  test("a smart flat with the acquisition mirror in fails generation, naming the row"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: SMART_FLAT, ${spectroscopy(1)} }, { kind: SMART_FLAT, ${imaging(1)} }")
      _   <- expectOdbError(
        user     = staff,
        query    = executionQuery(oid),
        expected = { case OdbError.SequenceUnavailable(_, Some(m)) if m.startsWith("Calibration set 2 (Smart Flat): missing Smart GCAL mapping") => () }
      )
    yield ()
