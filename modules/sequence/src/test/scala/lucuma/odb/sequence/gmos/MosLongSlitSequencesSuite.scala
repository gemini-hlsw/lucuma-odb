// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos

import cats.Eval
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.enums.GmosLongSlitAcquisitionRoi
import lucuma.core.enums.GmosMosAcquisitionType
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.mos.DefaultTelescopeConfigs
import lucuma.core.model.sequence.gmos.longslit.DefaultSlitTelescopeConfigs
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos.spectroscopy.Config.Common
import munit.FunSuite

import java.util.UUID

/**
 * These tests verify that sequence generation for MOS and long slit are essentially the same.
 */
class MosLongSlitSequencesSuite extends FunSuite:

  private val SlitWidth: GmosCustomSlitWidth = GmosCustomSlitWidth.CustomWidth_1_00
  private val Fpu:       GmosNorthFpu        = GmosNorthFpu.LongSlit_1_00
  private val Grating:   GmosNorthGrating    = GmosNorthGrating.R831_G5302
  private val Filter:    Option[GmosNorthFilter] = GmosNorthFilter.GPrime.some

  private val Namespace: UUID = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val Oid:       Observation.Id = Observation.Id.fromLong(1L).get

  private val common: Common =
    Common(
      centralWavelength         = Wavelength.fromIntNanometers(500).get,
      exposureTimeMode          = ExposureTimeMode.SignalToNoiseMode(SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(100)), Wavelength.fromIntNanometers(500).get),
      defaultXBin               = GmosXBinning.One,
      explicitXBin              = none,
      defaultYBin               = GmosYBinning.Two,
      explicitYBin              = none,
      explicitAmpReadMode       = none,
      explicitAmpGain           = none,
      explicitRoi               = none,
      explicitWavelengthDithers = none
    )

  private val longSlit: longslit.Config.GmosNorth =
    longslit.Config.GmosNorth(
      Grating,
      Filter,
      Fpu,
      common,
      DefaultSlitTelescopeConfigs.telescopeConfigs,
      longslit.AcquisitionConfig.GmosNorth(
        common.exposureTimeMode,
        GmosNorthFilter.GPrime,
        none,
        GmosLongSlitAcquisitionRoi.Ccd2,
        none
      )
    )

  private val mosConfig: mos.Config.GmosNorth =
    mos.Config.GmosNorth(
      Grating,
      Filter,
      GmosFpuMask.Custom(ToBeDefined, SlitWidth),
      GmosMosAcquisitionType.MaskOut,
      mos.AcquisitionConfig.GmosNorth(
        ExposureTimeMode.TimeAndCountMode(30.secondTimeSpan, PosInt.unsafeFrom(10), Wavelength.fromIntNanometers(500).get),
        GmosNorthFilter.GPrime,
        none
      ),
      common,
      DefaultTelescopeConfigs
    )

  private val mosConfigAtLongSlitOffsets: mos.Config.GmosNorth =
    mosConfig.withTelescopeConfigs(longSlit.telescopeConfigs)

  private val expander: SmartGcalExpander[Eval, StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
    SmartGcalExpander.pure[Eval, StaticConfig.GmosNorth, DynamicConfig.GmosNorth]: (_, _, d) =>
      (d, StepConfig.Gcal(StepConfig.Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W), GcalFilter.None, GcalDiffuser.Ir, GcalShutter.Open), ObserveClass.NightCal)

  private val estimator: StepTimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
    new StepTimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]:
      override def estimateStep(
        static: StaticConfig.GmosNorth,
        last:   StepTimeEstimateCalculator.Last[DynamicConfig.GmosNorth],
        next:   ProtoStep[DynamicConfig.GmosNorth]
      ): StepEstimate = StepEstimate.Zero

  private val time: IntegrationTime =
    IntegrationTime(5.minTimeSpan, PosInt.unsafeFrom(12))

  private def generate(
    config: spectroscopy.Config[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]
  ): List[Atom[DynamicConfig.GmosNorth]] =
    spectroscopy.Science
      .gmosNorth[Eval](
        Oid,
        estimator,
        InitialConfigs.GmosNorthStatic,
        Namespace,
        expander,
        "test",
        config,
        Right(time),
        none
      )
      .value
      .fold(e => fail(s"could not generate: $e"), _.generate.take(6).toList)

  private def fpuOf(s: Step[DynamicConfig.GmosNorth]): Option[GmosFpuMask[GmosNorthFpu]] =
    s.instrumentConfig.fpu

  private def withoutFpu(a: Atom[DynamicConfig.GmosNorth]): Atom[DynamicConfig.GmosNorth] =
    a.copy(steps = a.steps.map(s => s.copy(instrumentConfig = s.instrumentConfig.copy(fpu = none))))

  test("MOS generates the same science sequence as the equivalent long slit"):
    val ls = generate(longSlit)
    val ms = generate(mosConfigAtLongSlitOffsets)

    assert(ls.nonEmpty, "expected a non-empty long slit sequence")
    assertEquals(ms.map(withoutFpu), ls.map(withoutFpu))

  test("MOS does not nod by default"):
    assertEquals(mosConfig.telescopeConfigs, DefaultTelescopeConfigs)

    val qs = generate(mosConfig).flatMap(_.steps.toList).map(_.telescopeConfig.offset.q)
    assert(qs.nonEmpty)
    assert(qs.forall(_ === Offset.Q.Zero), s"expected every MOS step on axis, got ${qs.distinct}")

  test("each mode's steps carry its own aperture"):
    val lsFpus = generate(longSlit).flatMap(_.steps.toList).map(fpuOf)
    val msFpus = generate(mosConfigAtLongSlitOffsets).flatMap(_.steps.toList).map(fpuOf)

    assert(lsFpus.nonEmpty)
    assertEquals(msFpus.length, lsFpus.length)
    assert(
      lsFpus.forall(_.contains(GmosFpuMask.Builtin(Fpu))),
      s"expected every long slit step to carry the builtin FPU, got ${lsFpus.distinct}"
    )
    assert(
      msFpus.forall(_.contains(GmosFpuMask.Custom(ToBeDefined, SlitWidth))),
      s"expected every MOS step to carry the custom mask, got ${msFpus.distinct}"
    )
