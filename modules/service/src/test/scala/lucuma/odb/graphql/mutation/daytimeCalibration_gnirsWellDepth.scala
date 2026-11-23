// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
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
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
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

class daytimeCalibration_gnirsWellDepth extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps with InsertSmartGcalOps:

  val staff       = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(staff, serviceUser)

  // One flat row per well depth for the long slit setup below, told apart by their step counts.
  private def smartRow(wellDepth: GnirsWellDepth, count: Int): Gnirs.TableRow =
    Gnirs.TableRow(
      PosLong.unsafeFrom(1),
      Gnirs.TableKey(
        GnirsPixelScale.PixelScale_0_15,
        GnirsGrating.D32.some,
        GnirsPrism.Mirror.some,
        BoundedInterval.unsafeOpenUpper(
          Wavelength.fromIntNanometers(900).get,
          Wavelength.fromIntNanometers(2560).get
        ),
        GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
        wellDepth
      ),
      SmartGcalValue(
        Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.IrGreyBodyHigh), GcalFilter.Nir, GcalDiffuser.Ir, GcalShutter.Open),
        GcalBaselineType.Night,
        NonNegInt.unsafeFrom(count),
        LegacyInstrumentConfig(TimeSpan.unsafeFromMicroseconds(10_000_000L), PosInt.unsafeFrom(1))
      )
    )

  private val smartRows: List[Gnirs.TableRow] =
    List(smartRow(GnirsWellDepth.Shallow, 1), smartRow(GnirsWellDepth.Deep, 3))

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    servicesFor(serviceUser).map(_(s)).use: services =>
      services.transactionally:
        smartRows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser(services.smartGcalService.insertGnirs(i, r))

  private val smartFlat: String =
    """
      {
        kind: SMART_FLAT
        gnirs: {
          exposure: { seconds: 1 }
          coadds: 1
          filter: ORDER3
          decker: SHORT_CAM_LONG_SLIT
          fpuSlit: LONG_SLIT_0_30
          acquisitionMirrorOut: { prism: MIRROR, grating: D32, wavelength: { micrometers: 2.2 } }
          camera: SHORT_BLUE
          readMode: BRIGHT
        }
      }
    """

  private def createQuery(pid: Program.Id, daytimeCalibration: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: { observingMode: { daytimeCalibration: $daytimeCalibration } }
        }) {
          observation { id }
        }
      }
    """

  private def create(pid: Program.Id, daytimeCalibration: String): IO[Observation.Id] =
    query(staff, createQuery(pid, daytimeCalibration))
      .map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  private def updateQuery(oid: Observation.Id, daytimeCalibration: String): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: { observingMode: { daytimeCalibration: $daytimeCalibration } }
          WHERE: { id: { EQ: ${oid.asJson} } }
        }) {
          observations { id }
        }
      }
    """

  private def update(oid: Observation.Id, daytimeCalibration: String): IO[Unit] =
    query(staff, updateQuery(oid, daytimeCalibration)).map: json =>
      assertEquals(json.hcursor.downFields("updateObservations", "observations").require[List[Json]].size, 1)

  private def expectRejected(query: String, fragment: String): IO[Unit] =
    expectOdbError(
      user     = staff,
      query    = query,
      expected = { case OdbError.InvalidArgument(Some(m)) if m.contains(fragment) => () }
    )

  private def modeJson(oid: Observation.Id): IO[Json] =
    query(staff, s"""
      query {
        observation(observationId: ${oid.asJson}) {
          observingMode { daytimeCalibration { instrument gnirs { wellDepth } ghost { resolutionMode } calibrationSets { count } } }
        }
      }
    """).map(_.hcursor.downFields("observation", "observingMode", "daytimeCalibration").require[Json])

  private def wellDepth(oid: Observation.Id): IO[Option[String]] =
    modeJson(oid).map(_.hcursor.downFields("gnirs", "wellDepth").as[String].toOption)

  private def staticWellDepth(oid: Observation.Id): IO[String] =
    query(staff, s"""
      query {
        executionConfig(observationId: ${oid.asJson}) {
          gnirs { static { wellDepth } }
        }
      }
    """).map(_.hcursor.downFields("executionConfig", "gnirs", "static", "wellDepth").require[String])

  private def enterEmptySequence(oid: Observation.Id, mutation: String): IO[Unit] =
    query(staff, s"""
      mutation {
        $mutation(input: ${input(oid, SequenceType.Science)}) {
          sequence { description }
        }
      }
    """).void

  private def enterSequence(oid: Observation.Id): IO[Unit] =
    query(staff, s"""
      mutation {
        replaceGnirsSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", gnirsDarkStep))}) {
          sequence { description }
        }
      }
    """).void

  test("shallow when not given"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, s"{ instrument: GNIRS, calibrationSets: [$smartFlat] }")
      _   <- assertIO(
        modeJson(oid),
        json"""{ "instrument": "GNIRS", "gnirs": { "wellDepth": "SHALLOW" }, "ghost": null, "calibrationSets": [{ "count": 1 }] }"""
      )
      _   <- assertIO(staticWellDepth(oid), "SHALLOW")
    yield ()

  test("deep reaches the static config and the Smart GCAL lookup"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, s"{ instrument: GNIRS, calibrationSets: [$smartFlat], gnirs: { wellDepth: DEEP } }")
      _   <- assertIO(
        modeJson(oid),
        json"""{ "instrument": "GNIRS", "gnirs": { "wellDepth": "DEEP" }, "ghost": null, "calibrationSets": [{ "count": 3 }] }"""
      )
      _   <- assertIO(staticWellDepth(oid), "DEEP")
    yield ()

  test("an edit changes it, and omitting it keeps it"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, s"{ instrument: GNIRS, calibrationSets: [$smartFlat] }")
      _   <- update(oid, "{ gnirs: { wellDepth: DEEP } }")
      _   <- assertIO(wellDepth(oid), Some("DEEP"))
      _   <- update(oid, "{ instrument: GNIRS }")
      _   <- update(oid, "{ gnirs: {} }")
      _   <- assertIO(wellDepth(oid), Some("DEEP"))
      _   <- assertIO(staticWellDepth(oid), "DEEP")
    yield ()

  test("it may not be null"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GNIRS }")
      _   <- expectOdbError(staff, updateQuery(oid, "{ gnirs: { wellDepth: null } }"), { case OdbError.InvalidArgument(_) => () })
      _   <- expectOdbError(staff, updateQuery(oid, "{ gnirs: null }"), { case OdbError.InvalidArgument(_) => () })
      _   <- assertIO(wellDepth(oid), Some("SHALLOW"))
    yield ()

  test("only GNIRS takes it"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(createQuery(pid, "{ instrument: GHOST, gnirs: { wellDepth: DEEP } }"), "only allowed when the instrument is GNIRS")
      oid <- create(pid, "{ instrument: GMOS_NORTH }")
      _   <- assertIO(wellDepth(oid), None)
      _   <- expectRejected(updateQuery(oid, "{ gnirs: { wellDepth: DEEP } }"), "only allowed when the instrument is GNIRS")
    yield ()

  test("an instrument switch defaults it for GNIRS and clears it otherwise"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GNIRS, gnirs: { wellDepth: DEEP } }")
      _   <- update(oid, "{ instrument: GHOST }")
      _   <- assertIO(wellDepth(oid), None)
      _   <- update(oid, "{ instrument: GNIRS }")
      _   <- assertIO(wellDepth(oid), Some("SHALLOW"))
      _   <- update(oid, "{ instrument: GMOS_NORTH }")
      _   <- update(oid, "{ instrument: GNIRS, gnirs: { wellDepth: DEEP } }")
      _   <- assertIO(wellDepth(oid), Some("DEEP"))
    yield ()

  test("a stored sequence keeps it and freezes it"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GNIRS, gnirs: { wellDepth: DEEP } }")
      _   <- enterSequence(oid)
      _   <- assertIO(staticWellDepth(oid), "DEEP")
      _   <- expectRejected(updateQuery(oid, "{ gnirs: { wellDepth: SHALLOW } }"), "Cannot change the observing mode")
      _   <- assertIO(wellDepth(oid), Some("DEEP"))
    yield ()

  test("a materialized sequence without atoms freezes it"):
    val frozen = "Cannot change the GNIRS well depth"
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GNIRS, gnirs: { wellDepth: DEEP } }")
      _   <- enterEmptySequence(oid, "replaceGnirsSequence")
      _   <- expectRejected(updateQuery(oid, "{ gnirs: { wellDepth: SHALLOW } }"), frozen)
      _   <- expectRejected(updateQuery(oid, "{ instrument: GMOS_NORTH }"), frozen)
      _   <- update(oid, "{ gnirs: { wellDepth: DEEP } }")
      _   <- assertIO(wellDepth(oid), Some("DEEP"))
      gid <- create(pid, "{ instrument: GMOS_NORTH }")
      _   <- enterEmptySequence(gid, "replaceGmosNorthSequence")
      _   <- expectRejected(updateQuery(gid, "{ instrument: GNIRS }"), frozen)
    yield ()

  private val longSlitDarkStep: String =
    """
      {
        instrumentConfig: {
          exposure: { seconds: 1 }
          coadds: 1
          filter: ORDER3
          decker: SHORT_CAM_LONG_SLIT
          fpuSlit: LONG_SLIT_0_30
          acquisitionMirrorOut: { prism: MIRROR, grating: D32, wavelength: { micrometers: 2.2 } }
          camera: SHORT_BLUE
          readMode: BRIGHT
        }
        stepConfig: { dark: true }
        observeClass: DAY_CAL
      }
    """

  private def insertedFlats(daytimeCalibration: String): IO[Int] =
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, daytimeCalibration)
      _   <- query(staff, s"""
        mutation {
          replaceGnirsSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", longSlitDarkStep))}) {
            sequence { description }
          }
        }
      """)
      ids <- insertReturningIdsAs(staff, oid, List(SmartGcalType.Flat))
    yield ids.size

  test("a Smart GCAL step inserted into a hand-entered sequence uses it"):
    for
      _ <- assertIO(insertedFlats("{ instrument: GNIRS }"), 1)
      _ <- assertIO(insertedFlats("{ instrument: GNIRS, gnirs: { wellDepth: DEEP } }"), 3)
    yield ()

  test("a clone keeps it"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GNIRS, gnirs: { wellDepth: DEEP } }")
      cid <- cloneObservationAs(staff, oid)
      _   <- assertIO(wellDepth(cid), Some("DEEP"))
    yield ()
