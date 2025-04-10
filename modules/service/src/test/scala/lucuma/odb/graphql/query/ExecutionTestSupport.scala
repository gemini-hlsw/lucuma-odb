// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptySet
import cats.effect.Clock
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.Generator
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session

trait ExecutionTestSupport extends OdbSuite with ObservingModeSetupOperations {

  val pi: User          = TestUsers.Standard.pi(1, 30)
  val pi2: User         = TestUsers.Standard.pi(2, 32)
  val serviceUser       = TestUsers.service(3)
  val staff: User       = TestUsers.Standard.staff(4, 33)

  override val validUsers: List[User] =
    List(pi, pi2, serviceUser, staff)

  val createProgram: IO[Program.Id] =
    createProgramAs(pi, "Sequence Testing")

  val key_0_50: TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    TableKey(
      GratingConfigKey(
        GmosNorthGrating.R831_G5302,
        GmosGratingOrder.One,
        BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
      ).some,
      GmosNorthFilter.RPrime.some,
      GmosNorthFpu.LongSlit_0_50.some,
      GmosXBinning.One,
      GmosYBinning.Two,
      GmosAmpGain.Low
    )

  // NB: 2x4, no filter
  val key_1_00: TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    TableKey(
      GratingConfigKey(
        GmosNorthGrating.R831_G5302,
        GmosGratingOrder.One,
        BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
      ).some,
      none,
      GmosNorthFpu.LongSlit_1_00.some,
      GmosXBinning.Two,
      GmosYBinning.Four,
      GmosAmpGain.Low
    )

  val key_5_00: TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    TableKey(
      GratingConfigKey(
        GmosNorthGrating.R831_G5302,
        GmosGratingOrder.One,
        BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
      ).some,
      GmosNorthFilter.RPrime.some,
      GmosNorthFpu.LongSlit_5_00.some,
      GmosXBinning.One,
      GmosYBinning.Two,
      GmosAmpGain.Low
    )

  val flat =
    SmartGcalValue(
      Gcal(
        Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
        GcalFilter.Gmos,
        GcalDiffuser.Ir,
        GcalShutter.Open
      ),
      GcalBaselineType.Night,
      PosInt.unsafeFrom(1),
      LegacyInstrumentConfig(
        TimeSpan.unsafeFromMicroseconds(1_000_000L)
      )
    )

  val arc =
    SmartGcalValue(
      Gcal(
        Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.CuArArc)),
        GcalFilter.None,
        GcalDiffuser.Visible,
        GcalShutter.Closed
      ),
      GcalBaselineType.Day,
      PosInt.unsafeFrom(1),
      LegacyInstrumentConfig(
        TimeSpan.unsafeFromMicroseconds(1_000_000L)
      )
    )

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val tableRows: List[TableRow.North] =
      List(
        TableRow(PosLong.unsafeFrom(1), key_0_50, flat),
        TableRow(PosLong.unsafeFrom(1), key_0_50, arc),
        TableRow(PosLong.unsafeFrom(1), key_1_00, flat),
        TableRow(PosLong.unsafeFrom(1), key_1_00, arc),
        TableRow(PosLong.unsafeFrom(1), key_5_00, flat),
        TableRow(PosLong.unsafeFrom(1), key_5_00, arc)
      )

    Enums.load(s).flatMap { e =>
      val services = Services.forUser(pi /* doesn't matter*/, e, None)(s)
      services.transactionally {
        tableRows.zipWithIndex.traverse_ { (r, i) =>
          services.smartGcalService.insertGmosNorth(i, r)
        }
      }
    }
  }

  def calibrationProgram(role: CalibrationRole): IO[Program.Id] =
    query(
      user = staff,
      query = s"""
        query {
          programs(
            WHERE: {
              calibrationRole: {
                EQ: ${role.tag.toScreamingSnakeCase}
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """
    ).flatMap: js =>
      js.hcursor
        .downFields("programs", "matches")
        .values.toList.flatMap(_.toList).head // grab the first / only match
        .hcursor
        .downField("id").as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  def twilightProgram: IO[Program.Id] =
    calibrationProgram(CalibrationRole.Twilight)

  def calibrationTargets(role: CalibrationRole): IO[List[Target.Id]] =
    calibrationProgram(role).flatMap: pid =>
      query(
        user = staff,
        query = s"""
          query {
            targets(
              WHERE: {
                program: {
                  id: { EQ: "$pid" }
                }
              }
            ) {
              matches {
                id
              }
            }
          }
        """
      ).flatMap: js =>
        js.hcursor
          .downFields("targets", "matches")
          .values.toList.flatMap(_.toList)
          .traverse(_.hcursor.downField("id").as[Target.Id])
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

  def twilightTargets: IO[List[Target.Id]] =
    calibrationTargets(CalibrationRole.Twilight)

  val GmosAtomQuery: String =
    s"""
      description
      observeClass
      steps {
        instrumentConfig {
          exposure { seconds }
          readout {
            xBin
            yBin
          }
          roi
          gratingConfig {
            grating
            wavelength { nanometers }
          }
          filter
          fpu { builtin }
          centralWavelength { nanometers }
        }
        stepConfig {
          stepType
          ... on Gcal {
            continuum
            arcs
          }
        }
        telescopeConfig {
          offset {
            p { arcseconds }
            q { arcseconds }
          }
          guiding
        }
        observeClass
        breakpoint
      }
    """

  def excutionConfigQuery(inst: String, sequenceType: String, atomQuery: String, futureLimit: Option[Int]): String =
    s"""
      execution {
        config${futureLimit.fold("")(lim => s"(futureLimit: $lim)")} {
          $inst {
            $sequenceType {
              nextAtom {
                $atomQuery
              }
              possibleFuture {
                $atomQuery
              }
              hasMore
            }
          }
        }
      }
    """

  def gmosNorthAcquisitionQuery(futureLimit: Option[Int]): String =
    excutionConfigQuery("gmosNorth", "acquisition", GmosAtomQuery, futureLimit)

  def gmosNorthScienceQuery(futureLimit: Option[Int]): String =
    excutionConfigQuery("gmosNorth", "science", GmosAtomQuery, futureLimit)

  val ObsWavelength: Wavelength =
    Wavelength.decimalNanometers.unsafeGet(BigDecimal("500.0"))

  def obsWavelengthAt(ditherNm: Int): Wavelength =
    ObsWavelength.unsafeOffset(
      WavelengthDither.decimalNanometers.unsafeGet(BigDecimal(ditherNm))
    )

  def gmosNorthScience(ditherNm: Int): GmosNorth =
    GmosNorth(
      fakeItcSpectroscopyResult.exposureTime,
      GmosCcdMode(GmosXBinning.One, GmosYBinning.Two, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Slow),
      GmosDtax.Zero,
      GmosRoi.FullFrame,
      GmosGratingConfig.North(GmosNorthGrating.R831_G5302, GmosGratingOrder.One, obsWavelengthAt(ditherNm)).some,
      GmosNorthFilter.RPrime.some,
      GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_0_50).some
    )

  def gmosNorthArc(ditherNm: Int): GmosNorth =
    gmosNorthScience(ditherNm).copy(exposure = arc.instrumentConfig.exposureTime)

  def gmosNorthFlat(ditherNm: Int): GmosNorth =
    gmosNorthScience(ditherNm).copy(exposure = flat.instrumentConfig.exposureTime)

  val GmosNorthAcq0: GmosNorth =
    gmosNorthScience(0).copy(
      exposure      = fakeItcImagingResult.exposureTime,
      readout       = GmosCcdMode(GmosXBinning.Two, GmosYBinning.Two, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      roi           = GmosRoi.Ccd2,
      gratingConfig = none,
      filter        = GmosNorthFilter.GPrime.some,
      fpu           = none
    )

  val GmosNorthAcq1: GmosNorth =
    GmosNorthAcq0.copy(
      exposure = GmosNorthAcq0.exposure *| 2,
      readout  = GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      roi      = GmosRoi.CentralStamp,
      fpu      = gmosNorthScience(0).fpu
    )

  val GmosNorthAcq2: GmosNorth =
    GmosNorthAcq1.copy(
      exposure = GmosNorthAcq0.exposure *| 3
    )

  def gmosNorthAcq(step: Int): GmosNorth =
    step match
      case 0 => GmosNorthAcq0
      case 1 => GmosNorthAcq1
      case 2 => GmosNorthAcq2
      case _ => sys.error("Only 3 steps in a GMOS North Acq")

  val FlatStep: StepConfig.Gcal =
    StepConfig.Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W), GcalFilter.Gmos, GcalDiffuser.Ir, GcalShutter.Open)

  val ArcStep: StepConfig.Gcal  =
    StepConfig.Gcal(Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.CuArArc)), GcalFilter.None, GcalDiffuser.Visible, GcalShutter.Closed)

  def telescopeConfig(p: Int, q: Int, g: StepGuideState): TelescopeConfig =
    TelescopeConfig(
      Offset(
        Offset.P.signedDecimalArcseconds.reverseGet(BigDecimal(p)),
        Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(q))
      ),
      g
    )

  protected def expectedTelescopeConfig(p: Int, q: Int, g: StepGuideState): Json =
    expectedTelescopeConfig(telescopeConfig(p, q, g))

  protected def expectedTelescopeConfig(t: TelescopeConfig): Json =
    json"""
      {
        "offset": {
          "p": { "arcseconds": ${Angle.signedDecimalArcseconds.get(t.offset.p.toAngle)} },
          "q": { "arcseconds": ${Angle.signedDecimalArcseconds.get(t.offset.q.toAngle)} }
        },
        "guiding": ${t.guiding}
      }
    """

  protected def gmosNorthExpectedInstrumentConfig(gn: GmosNorth): Json =
    json"""
      {
        "exposure": { "seconds": ${gn.exposure.toSeconds} },
        "readout": {
          "xBin": ${gn.readout.xBin},
          "yBin": ${gn.readout.yBin}
        },
        "roi": ${gn.roi},
        "gratingConfig": ${gn.gratingConfig.fold(Json.Null) { gc =>
          json"""
            {
              "grating": ${gc.grating},
              "wavelength": {
                "nanometers": ${gc.wavelength.toNanometers.value.value}
              }
            }
          """
        }},
        "filter": ${gn.filter},
        "fpu": ${gn.fpu.fold(Json.Null) { fpu =>
          json"""{ "builtin": ${fpu.builtin.map(_.value)} }"""
        }},
        "centralWavelength": ${gn.centralWavelength.fold(Json.Null) { cw =>
          json"""
            {
              "nanometers": ${cw.toNanometers.value.value}
            }
          """
        }}
      }
    """

  protected def gmosNorthExpectedArc(ditherNm: Int, p: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig" : ${gmosNorthExpectedInstrumentConfig(gmosNorthArc(ditherNm))},
        "stepConfig" : {
          "stepType": "GCAL",
          "continuum" : null,
          "arcs" : ${arc.gcalConfig.lamp.arcs.map(_.toList) }
        },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, StepGuideState.Disabled)},
        "observeClass" : "NIGHT_CAL",
        "breakpoint": "DISABLED"
      }
    """

  protected def gmosNorthExpectedFlat(ditherNm: Int, p: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig" : ${gmosNorthExpectedInstrumentConfig(gmosNorthFlat(ditherNm))},
        "stepConfig" : {
          "stepType": "GCAL",
          "continuum" : ${flat.gcalConfig.lamp.continuum},
          "arcs" : []
        },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, StepGuideState.Disabled)},
        "observeClass" : "NIGHT_CAL",
        "breakpoint": "DISABLED"
      }
    """

  protected def gmosNorthExpectedScience(ditherNm: Int, p: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig" : ${gmosNorthExpectedInstrumentConfig(gmosNorthScience(ditherNm))},
        "stepConfig" : { "stepType": "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, StepGuideState.Enabled)},
        "observeClass" : "SCIENCE",
        "breakpoint": "DISABLED"
      }
    """

  protected def gmosNorthExpectedAcq(step: Int, p: Int, breakpoint: Breakpoint = Breakpoint.Disabled): Json =
    json"""
      {
        "instrumentConfig" : ${gmosNorthExpectedInstrumentConfig(gmosNorthAcq(step))},
        "stepConfig" : { "stepType":  "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(p, 0, StepGuideState.Enabled)},
        "observeClass" : "ACQUISITION",
        "breakpoint": ${breakpoint.tag.toScreamingSnakeCase.asJson}
      }
    """

  protected def gmosNorthExpectedScienceAtom(ditherNm: Int, p: Int, q: Int, exposures: Int): Json =
    val steps = List(
      gmosNorthExpectedArc(ditherNm, p, q), gmosNorthExpectedFlat(ditherNm, p, q)
    ) ++ List.fill(exposures)(gmosNorthExpectedScience(ditherNm, p, q))

    Json.obj(
      "description" -> s"$ditherNm.000 nm, $q.000000″".asJson,
      "observeClass" -> "SCIENCE".asJson,
      "steps" -> steps.asJson
    )

  def addEndStepEvent(sid: Step.Id): IO[Unit] = {
    val q = s"""
      mutation {
        addStepEvent(input: {
          stepId: "$sid",
          stepStage: END_STEP
        }) {
          event {
            step {
              id
            }
          }
        }
      }
    """

    query(serviceUser, q).void
  }

  def addDatasetEvent(did: Dataset.Id, stage: DatasetStage): IO[Unit] = {
    val q = s"""
      mutation {
        addDatasetEvent(input: {
          datasetId:    "$did",
          datasetStage: ${stage.tag.toUpperCase}
        }) {
          event {
            id
          }
        }
      }
    """

    query(serviceUser, q).void
  }

  def setQaState(did: Dataset.Id, qa: DatasetQaState): IO[Unit] = {
    val q = s"""
        mutation {
          updateDatasets(input: {
            SET: {
              qaState: ${qa.tag.toUpperCase}
            },
            WHERE: {
              id: { EQ: "$did" }
            }
          }) {
            datasets {
              id
            }
          }
        }
    """

    query(serviceUser, q).void
  }

  /**
   * What time is it now, as a Timestamp.
   */
  def timestampNow: IO[Timestamp] =
    Clock[IO]
      .realTimeInstant
      .map(Timestamp.fromInstantTruncated)
      .flatMap(t => IO.fromOption(t)(new RuntimeException("oddly, timestamp of now is out of range")))

  /**
   * Generates the sequence for the given observation.
   *
   * @param limit the future limit, which must be in the range [0, 100]
   * @param when the timestamp to pass the generator. in other words generate as
   *             if asked at this time
   */
  def generate(
    pid:      Program.Id,
    oid:      Observation.Id,
    limit:    Option[Int]       = None,  // [0, 100]
    when:     Option[Timestamp] = None
  ): IO[Either[Generator.Error, InstrumentExecutionConfig]] =
    withSession: session =>
      for
        future <- limit.traverse(lim => IO.fromOption(Generator.FutureLimit.from(lim).toOption)(new IllegalArgumentException("Specify a future limit from 0 to 100")))
        enums  <- Enums.load(session)
        tec    <- TimeEstimateCalculatorImplementation.fromSession(session, enums)
        srv     = Services.forUser(serviceUser, enums, None)(session)
        gen     = srv.generator(CommitHash.Zero, itcClient, tec)
        res    <- gen.generate(pid, oid, future.getOrElse(Generator.FutureLimit.Default), when)
      yield res

  /**
   * Generates the sequence but fails if it produces an error.
   *
   * @param limit the future limit, which must be in the range [0, 100]
   * @param when the timestamp to pass the generator. in other words generate as
   *             if asked at this time
   */
  def generateOrFail(
    pid:      Program.Id,
    oid:      Observation.Id,
    limit:    Option[Int]       = None,  // [0, 100]
    when:     Option[Timestamp] = None
  ): IO[InstrumentExecutionConfig] =
    generate(pid, oid, limit, when).flatMap: res =>
      IO.fromEither(res.leftMap(e => new RuntimeException(s"Failed to generate the sequence: ${e.format}")))

  /**
   * Generates the sequence as if requested after the specified amount of time
   * has passed.
   *
   * @param time amount of time (from now) to use as a timestamp for sequence
   *             generation; does not delay the computation in any way
   */
  def generateAfter(
    pid:  Program.Id,
    oid:  Observation.Id,
    time: TimeSpan
  ): IO[Either[Generator.Error, InstrumentExecutionConfig]] =
    for {
      now  <- timestampNow
      when <- IO.fromOption(now.plusMicrosOption(time.toMicroseconds))(new IllegalArgumentException(s"$time is too big"))
      res  <- generate(pid, oid, when = when.some)
    } yield res

  /**
   * Generates the sequence as if requested after the specified amount of time
   * has passed, fails if the sequence cannot be generated.
   *
   * @param time amount of time (from now) to use as a timestamp for sequence
   *             generation; does not delay the computation in any way
   */
  def generateAfterOrFail(
    pid:  Program.Id,
    oid:  Observation.Id,
    time: TimeSpan
  ): IO[InstrumentExecutionConfig] =
    generateAfter(pid, oid, time).flatMap: res =>
      IO.fromEither(res.leftMap(e => new RuntimeException(s"Failed to generate the sequence: ${e.format}")))

}