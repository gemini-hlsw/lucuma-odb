// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.StepGuideState
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session

trait ExecutionTestSupportForFlamingos2 extends ExecutionTestSupport:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.secondTimeSpan,
      NonNegInt.unsafeFrom(4)
    )

  val f2_key_JH1: Flamingos2.TableKey =
    Flamingos2.TableKey(
      Flamingos2Disperser.R1200JH.some,
      Flamingos2Filter.JH,
      Flamingos2Fpu.LongSlit1.some
    )

  val f2_flat_JH1 =
    SmartGcalValue(
      Gcal(
        Gcal.Lamp.fromContinuum(GcalContinuum.IrGreyBodyHigh),
        GcalFilter.Nd20,
        GcalDiffuser.Ir,
        GcalShutter.Open
      ),
      GcalBaselineType.Night,
      PosInt.unsafeFrom(1),
      LegacyInstrumentConfig(
        TimeSpan.unsafeFromMicroseconds(15_000_000L)
      )
    )

  val f2_arc_JH1 =
    SmartGcalValue(
      Gcal(
        Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.ArArc)),
        GcalFilter.Nir,
        GcalDiffuser.Ir,
        GcalShutter.Closed
      ),
      GcalBaselineType.Night,
      PosInt.unsafeFrom(1),
      LegacyInstrumentConfig(
        TimeSpan.unsafeFromMicroseconds(32_000_000L)
      )
    )

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val rows: List[Flamingos2.TableRow] =
      List(
        Flamingos2.TableRow(PosLong.unsafeFrom(1), f2_key_JH1, f2_flat_JH1),
        Flamingos2.TableRow(PosLong.unsafeFrom(1), f2_key_JH1, f2_arc_JH1)
      )

    Enums.load(s).flatMap: e =>
      val services = Services.forUser(pi /* doesn't matter*/, e, None)(s)
      services.transactionally:
        rows.zipWithIndex.traverse_ : (r, i) =>
          services.smartGcalService.insertFlamingos2(i, r)
  }

  val Flamingos2AtomQuery: String =
    s"""
      description
      observeClass
      steps {
        instrumentConfig {
          exposure { seconds }
          disperser
          filter
          readMode
          lyotWheel
          fpu { builtin }
          decker
          readoutMode
          reads
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

  def flamingos2AcquisitionQuery(futureLimit: Option[Int]): String =
    excutionConfigQuery("flamingos2", "acquisition", Flamingos2AtomQuery, futureLimit)

  def flamingos2ScienceQuery(futureLimit: Option[Int]): String =
    excutionConfigQuery("flamingos2", "science", Flamingos2AtomQuery, futureLimit)

  val Flamingos2Science: Flamingos2DynamicConfig =
    Flamingos2DynamicConfig(
      fakeItcSpectroscopyResult.exposureTime,
      Flamingos2Disperser.R1200JH.some,
      Flamingos2Filter.JH,
      Flamingos2ReadMode.Bright,
      Flamingos2LyotWheel.F16,
      Flamingos2FpuMask.Builtin(Flamingos2Fpu.LongSlit1),
      Flamingos2Decker.LongSlit,
      Flamingos2ReadoutMode.Science,
      Flamingos2ReadMode.Bright.readCount
    )

  val Flamingos2Arc: Flamingos2DynamicConfig =
    Flamingos2Science.copy(exposure = f2_arc_JH1.instrumentConfig.exposureTime)

  val Flamingos2Flat: Flamingos2DynamicConfig =
    Flamingos2Science.copy(exposure = f2_flat_JH1.instrumentConfig.exposureTime)

  val Flamingos2Acq0: Flamingos2DynamicConfig =
    Flamingos2Science.copy(
      exposure  = fakeItcImagingResult.exposureTime,
      disperser = none,
      filter    = Flamingos2Filter.J,
      fpu       = Flamingos2FpuMask.Imaging,
      decker    = Flamingos2Decker.Imaging
    )

  val Flamingos2Acq1: Flamingos2DynamicConfig =
    Flamingos2Acq0.copy(
      exposure  = 10.secondTimeSpan,
      fpu       = Flamingos2FpuMask.Builtin(Flamingos2Fpu.LongSlit1),
      decker    = Flamingos2Decker.LongSlit
    )

  val Flamingos2Acq2: Flamingos2DynamicConfig =
    Flamingos2Acq1.copy(
      exposure = fakeItcImagingResult.exposureTime
    )

  def flamingos2Acq(step: Int): Flamingos2DynamicConfig =
    step match
      case 0 => Flamingos2Acq0
      case 1 => Flamingos2Acq1
      case 2 => Flamingos2Acq2
      case _ => sys.error("Only 3 steps in a Flamingos 2 Acq")

  val Flamingos2FlatStep: StepConfig.Gcal =
    f2_flat_JH1.gcalConfig

  val Flamingos2ArcStep: StepConfig.Gcal  =
    f2_arc_JH1.gcalConfig

  protected def flamingos2ExpectedInstrumentConfig(f2: Flamingos2DynamicConfig): Json =
    json"""
      {
        "exposure": { "seconds": ${f2.exposure.toSeconds} },
        "disperser": ${f2.disperser.fold(Json.Null)(_.asJson)},
        "filter": ${f2.filter},
        "readMode": ${f2.readMode},
        "lyotWheel": ${f2.lyotWheel},
        "fpu": ${
          f2.fpu match
            case Flamingos2FpuMask.Imaging      => Json.Null
            case Flamingos2FpuMask.Builtin(f)   => json"""{ "builtin": $f }"""
            case Flamingos2FpuMask.Custom(f, w) => json"""{ "filename": ${f.value}, "slitWidth": $w }"""
        },
        "decker": ${f2.decker},
        "readoutMode": ${f2.readoutMode},
        "reads": ${f2.reads}
      }
    """

  protected def flamingos2ExpectedAcq(step: Int, p: Int, breakpoint: Breakpoint = Breakpoint.Disabled): Json =
    json"""
      {
        "instrumentConfig" : ${flamingos2ExpectedInstrumentConfig(flamingos2Acq(step))},
        "stepConfig" : { "stepType":  "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(p, 0, StepGuideState.Enabled)},
        "observeClass" : "ACQUISITION",
        "breakpoint": ${breakpoint.tag.toScreamingSnakeCase.asJson}
      }
    """

  protected def flamingos2ExpectedArc(p: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig": ${flamingos2ExpectedInstrumentConfig(Flamingos2Arc)},
        "stepConfig" : {
          "stepType": "GCAL",
          "continuum" : null,
          "arcs" : ${f2_arc_JH1.gcalConfig.lamp.arcs.map(_.toList) }
        },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, StepGuideState.Disabled)},
        "observeClass" : "NIGHT_CAL",
        "breakpoint": "DISABLED"
      }
    """

  protected def flamingos2ExpectedFlat(p: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig" : ${flamingos2ExpectedInstrumentConfig(Flamingos2Flat)},
        "stepConfig" : {
          "stepType": "GCAL",
          "continuum" : ${f2_flat_JH1.gcalConfig.lamp.continuum},
          "arcs" : []
        },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, StepGuideState.Disabled)},
        "observeClass" : "NIGHT_CAL",
        "breakpoint": "DISABLED"
      }

    """

  protected def flamingos2ExpectedScience(p: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig": ${flamingos2ExpectedInstrumentConfig(Flamingos2Science)},
        "stepConfig": { "stepType": "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, StepGuideState.Enabled)},
        "observeClass": "SCIENCE",
        "breakpoint": "DISABLED"
      }
    """

  protected def flamingos2ExpectedScienceAtom(a: (Int, Int), b: (Int, Int)): Json =
    val sciSteps  = List(a, b, b, a).map((p, q) => flamingos2ExpectedScience(p, q))
    val gcalSteps = List(flamingos2ExpectedFlat(a._1, a._2), flamingos2ExpectedArc(a._1, a._2))

    Json.obj(
      "description" -> s"Long Slit 1px, JH, R1200JH".asJson,
      "observeClass" -> "SCIENCE".asJson,
      "steps" -> (sciSteps ++ gcalSteps).asJson
    )