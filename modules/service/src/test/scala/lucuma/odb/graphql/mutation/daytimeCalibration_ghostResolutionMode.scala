// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.foldable.*
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
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostResolutionMode
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

class daytimeCalibration_ghostResolutionMode extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val staff       = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(staff, serviceUser)

  // One flat row per resolution mode, told apart by their step counts.
  private def smartRow(resolutionMode: GhostResolutionMode, count: Int): Ghost.TableRow =
    val exposure = TimeSpan.unsafeFromMicroseconds(10_000_000L)
    Ghost.TableRow(
      Ghost.SearchKey(resolutionMode, GhostBinning.OneByOne, GhostBinning.OneByOne),
      SmartGcalValue(
        Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen100W), GcalFilter.None, GcalDiffuser.Visible, GcalShutter.Open),
        GcalBaselineType.Night,
        NonNegInt.unsafeFrom(count),
        Ghost.GhostUpdate(exposure, PosInt.unsafeFrom(1), exposure, PosInt.unsafeFrom(1))
      )
    )

  private val smartRows: List[Ghost.TableRow] =
    List(smartRow(GhostResolutionMode.Standard, 1), smartRow(GhostResolutionMode.High, 3))

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    servicesFor(serviceUser).map(_(s)).use: services =>
      services.transactionally:
        smartRows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser(services.smartGcalService.insertGhost(i, PosLong.unsafeFrom(i + 1), r))

  private val smartFlat: String =
    """
      {
        kind: SMART_FLAT
        ghost: {
          red:  { exposureTime: { seconds: 1 }, exposureCount: 1, binning: ONE_BY_ONE, readMode: MEDIUM }
          blue: { exposureTime: { seconds: 1 }, exposureCount: 1, binning: ONE_BY_ONE, readMode: MEDIUM }
          ifu1FiberAgitator: DISABLED
          ifu2FiberAgitator: DISABLED
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
          observingMode { daytimeCalibration { instrument ghost { resolutionMode } calibrationSets { count } } }
        }
      }
    """).map(_.hcursor.downFields("observation", "observingMode", "daytimeCalibration").require[Json])

  private def resolutionMode(oid: Observation.Id): IO[Option[String]] =
    modeJson(oid).map(_.hcursor.downFields("ghost", "resolutionMode").as[String].toOption)

  private def staticResolutionMode(oid: Observation.Id): IO[String] =
    query(staff, s"""
      query {
        executionConfig(observationId: ${oid.asJson}) {
          ghost { static { resolutionMode ifuMapping { mappingType } } }
        }
      }
    """).map: json =>
      val static = json.hcursor.downFields("executionConfig", "ghost", "static")
      assertEquals(static.downFields("ifuMapping", "mappingType").require[String], "NO_TARGET")
      static.downField("resolutionMode").require[String]

  private def enterSequence(oid: Observation.Id): IO[Unit] =
    query(staff, s"""
      mutation {
        replaceGhostSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", ghostDarkStep))}) {
          sequence { description }
        }
      }
    """).void

  test("standard when not given"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, s"{ instrument: GHOST, calibrationSets: [$smartFlat] }")
      _   <- assertIO(resolutionMode(oid), Some("STANDARD"))
      _   <- assertIO(staticResolutionMode(oid), "STANDARD")
    yield ()

  test("high resolution reaches the static config and the Smart GCAL lookup"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, s"{ instrument: GHOST, calibrationSets: [$smartFlat], ghost: { resolutionMode: HIGH } }")
      _   <- assertIO(
        modeJson(oid),
        json"""{ "instrument": "GHOST", "ghost": { "resolutionMode": "HIGH" }, "calibrationSets": [{ "count": 3 }] }"""
      )
      _   <- assertIO(staticResolutionMode(oid), "HIGH")
    yield ()

  test("an edit changes it, and omitting it keeps it"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, s"{ instrument: GHOST, calibrationSets: [$smartFlat] }")
      _   <- update(oid, "{ ghost: { resolutionMode: HIGH } }")
      _   <- assertIO(
        modeJson(oid),
        json"""{ "instrument": "GHOST", "ghost": { "resolutionMode": "HIGH" }, "calibrationSets": [{ "count": 3 }] }"""
      )
      _   <- update(oid, "{ instrument: GHOST }")
      _   <- update(oid, "{ ghost: {} }")
      _   <- assertIO(resolutionMode(oid), Some("HIGH"))
      _   <- assertIO(staticResolutionMode(oid), "HIGH")
    yield ()

  test("it may not be null"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GHOST }")
      _   <- expectOdbError(staff, updateQuery(oid, "{ ghost: { resolutionMode: null } }"), { case OdbError.InvalidArgument(_) => () })
      _   <- expectOdbError(staff, updateQuery(oid, "{ ghost: null }"), { case OdbError.InvalidArgument(_) => () })
      _   <- assertIO(resolutionMode(oid), Some("STANDARD"))
    yield ()

  test("only GHOST takes it"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(createQuery(pid, "{ instrument: GMOS_NORTH, ghost: { resolutionMode: HIGH } }"), "only allowed when the instrument is GHOST")
      oid <- create(pid, "{ instrument: GMOS_NORTH }")
      _   <- assertIO(resolutionMode(oid), None)
      _   <- expectRejected(updateQuery(oid, "{ ghost: { resolutionMode: HIGH } }"), "only allowed when the instrument is GHOST")
    yield ()

  test("an instrument switch defaults it for GHOST and clears it otherwise"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GHOST, ghost: { resolutionMode: HIGH } }")
      _   <- update(oid, "{ instrument: GMOS_NORTH }")
      _   <- assertIO(resolutionMode(oid), None)
      _   <- update(oid, "{ instrument: GHOST }")
      _   <- assertIO(resolutionMode(oid), Some("STANDARD"))
      _   <- update(oid, "{ instrument: GMOS_NORTH }")
      _   <- update(oid, "{ instrument: GHOST, ghost: { resolutionMode: HIGH } }")
      _   <- assertIO(resolutionMode(oid), Some("HIGH"))
    yield ()

  test("a stored sequence keeps it and freezes it"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GHOST, ghost: { resolutionMode: HIGH } }")
      _   <- enterSequence(oid)
      _   <- assertIO(staticResolutionMode(oid), "HIGH")
      _   <- expectRejected(updateQuery(oid, "{ ghost: { resolutionMode: STANDARD } }"), "with a stored sequence")
      _   <- assertIO(resolutionMode(oid), Some("HIGH"))
    yield ()

  test("a clone keeps it"):
    for
      pid <- createProgramAs(staff)
      oid <- create(pid, "{ instrument: GHOST, ghost: { resolutionMode: HIGH } }")
      cid <- cloneObservationAs(staff, oid)
      _   <- assertIO(resolutionMode(cid), Some("HIGH"))
    yield ()
