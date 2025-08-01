// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
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
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session

trait ExecutionTestSupportForGmos extends ExecutionTestSupport:

  val gn_key_0_50: Gmos.TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    Gmos.TableKey(
      Gmos.GratingConfigKey(
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
  val gn_key_1_00: Gmos.TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    Gmos.TableKey(
      Gmos.GratingConfigKey(
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

  val gn_key_5_00: Gmos.TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    Gmos.TableKey(
      Gmos.GratingConfigKey(
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

  val gn_flat =
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

  val gn_arc =
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
    val rows: List[Gmos.TableRow.North] =
      List(
        Gmos.TableRow(PosLong.unsafeFrom(1), gn_key_0_50, gn_flat),
        Gmos.TableRow(PosLong.unsafeFrom(1), gn_key_0_50, gn_arc),
        Gmos.TableRow(PosLong.unsafeFrom(1), gn_key_1_00, gn_flat),
        Gmos.TableRow(PosLong.unsafeFrom(1), gn_key_1_00, gn_arc),
        Gmos.TableRow(PosLong.unsafeFrom(1), gn_key_5_00, gn_flat),
        Gmos.TableRow(PosLong.unsafeFrom(1), gn_key_5_00, gn_arc)
      )

    Enums.load(s).flatMap: e =>
      val services = Services.forUser(pi /* doesn't matter*/, e, None)(s)
      services.transactionally:
        rows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser:
            services.smartGcalService.insertGmosNorth(i, r)
  }

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

  def gmosNorthAcquisitionQuery(oid: Observation.Id, futureLimit: Option[Int] = None): String =
    executionConfigQuery(oid, "gmosNorth", "acquisition", GmosAtomQuery, futureLimit)

  def gmosNorthScienceQuery(oid: Observation.Id, futureLimit: Option[Int] = None): String =
    executionConfigQuery(oid, "gmosNorth", "science", GmosAtomQuery, futureLimit)

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
    gmosNorthScience(ditherNm).copy(exposure = gn_arc.instrumentConfig.exposureTime)

  def gmosNorthFlat(ditherNm: Int): GmosNorth =
    gmosNorthScience(ditherNm).copy(exposure = gn_flat.instrumentConfig.exposureTime)

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
          "arcs" : ${gn_arc.gcalConfig.lamp.arcs.map(_.toList) }
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
          "continuum" : ${gn_flat.gcalConfig.lamp.continuum},
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