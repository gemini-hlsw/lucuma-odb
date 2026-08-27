// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2

import cats.Eval
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Pure
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.flamingos2.spectroscopy.AcquisitionConfig
import lucuma.odb.sequence.flamingos2.spectroscopy.Config.Common
import munit.FunSuite

import java.util.UUID

/**
 * Flamingos 2 MOS and long slit generate the same science sequence apart from the
 * aperture, but calibrate on their own cadences: 90 minutes for long slit, 2 hours
 * for MOS.  The per-step aperture is covered by the executionSciFlamingos2Mos
 * GraphQL suite; the cadence boundary is only reachable here.
 */
class MosLongSlitSequencesSuite extends FunSuite:

  private val SlitWidth: Flamingos2CustomSlitWidth = Flamingos2CustomSlitWidth.CustomWidth_2_pix
  private val Fpu:       Flamingos2Fpu             = Flamingos2Fpu.LongSlit2
  private val Disperser: Flamingos2Disperser       = Flamingos2Disperser.R1200HK
  private val Filter:    Flamingos2Filter          = Flamingos2Filter.HK

  private val Namespace: UUID           = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val Oid:       Observation.Id = Observation.Id.fromLong(1L).get

  private val ExposureTime: TimeSpan = 5.minTimeSpan

  private def offsetQ(arcsec: Int): TelescopeConfig =
    TelescopeConfig(Offset(Offset.P.Zero, Offset.Q(Angle.fromDoubleArcseconds(arcsec.toDouble))), StepGuideState.Enabled)

  private val abbaOffsets: NonEmptyList[TelescopeConfig] =
    NonEmptyList.of(offsetQ(15), offsetQ(-15), offsetQ(-15), offsetQ(15))

  private val common: Common =
    Common(
      exposureTimeMode    = ExposureTimeMode.SignalToNoiseMode(SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(100)), Wavelength.fromIntNanometers(2000).get),
      explicitReadMode    = none,
      explicitReads       = none,
      explicitDecker      = none,
      defaultReadoutMode  = Flamingos2ReadoutMode.Science,
      explicitReadoutMode = none,
      telescopeConfigs    = abbaOffsets,
      telluricType        = lucuma.core.model.TelluricType.Hot
    )

  private val acquisition: AcquisitionConfig =
    AcquisitionConfig(common.exposureTimeMode, Filter, none)

  // Unlike long slit, MOS acquisition is sized directly from a Time & Count mode.
  private val mosAcquisition: AcquisitionConfig =
    AcquisitionConfig(
      ExposureTimeMode.TimeAndCountMode(30.secondTimeSpan, PosInt.unsafeFrom(1), Wavelength.fromIntNanometers(2000).get),
      Filter,
      none
    )

  private val longSlit: longslit.Config =
    longslit.Config(
      Disperser,
      Filter,
      Fpu,
      acquisition,
      common,
      none
    )

  private val mosConfig: mos.Config =
    mos.Config(
      Disperser,
      Filter,
      Flamingos2FpuMask.Custom(ToBeDefined, SlitWidth),
      mosAcquisition,
      common
    ).fold(m => fail(s"could not build the MOS config: $m"), identity)

  private val expander: SmartGcalExpander[Eval, Flamingos2StaticConfig, Flamingos2DynamicConfig] =
    SmartGcalExpander.pure[Eval, Flamingos2StaticConfig, Flamingos2DynamicConfig]: (_, _, d) =>
      (d, StepConfig.Gcal(StepConfig.Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W), GcalFilter.None, GcalDiffuser.Ir, GcalShutter.Open), ObserveClass.NightCal)

  // Each step costs its own exposure time, so an ABBA cycle costs 4 x ExposureTime.
  private val estimator: StepTimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig] =
    new StepTimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig]:
      override def estimateStep(
        static: Flamingos2StaticConfig,
        last:   StepTimeEstimateCalculator.Last[Flamingos2DynamicConfig],
        next:   ProtoStep[Flamingos2DynamicConfig]
      ): StepEstimate =
        StepEstimate.fromMax(List(ConfigChangeEstimate("test", "test", next.value.exposure)), Nil)

  private val CycleEstimate: TimeSpan = ExposureTime *| 4

  private def itc(cycles: Int): Either[OdbError, IntegrationTime] =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(cycles * 4)).asRight

  private def science(
    gen: Eval[Either[OdbError, StreamingExecutionConfig[Pure, Flamingos2StaticConfig, Flamingos2DynamicConfig]]]
  ): List[Atom[Flamingos2DynamicConfig]] =
    gen.value.fold(e => fail(s"could not generate: $e"), _.science.toList)

  private def generateLongSlit(cycles: Int): List[Atom[Flamingos2DynamicConfig]] =
    science(longslit.LongSlit.instantiate[Eval](Oid, estimator, Namespace, expander, longSlit, itc(cycles), itc(cycles), none))

  private def generateMos(cycles: Int): List[Atom[Flamingos2DynamicConfig]] =
    science(mos.Mos.instantiate[Eval](Oid, estimator, Namespace, expander, mosConfig, itc(cycles), none))

  // The aperture and the decker that follows from it are what the two modes differ in.
  private def normalized(a: Atom[Flamingos2DynamicConfig]): Atom[Flamingos2DynamicConfig] =
    a.copy(steps = a.steps.map: s =>
      s.copy(instrumentConfig = s.instrumentConfig.copy(fpu = Flamingos2FpuMask.Imaging, decker = Flamingos2Decker.Imaging))
    )

  private def titles(as: List[Atom[Flamingos2DynamicConfig]]): List[String] =
    as.map(_.description.fold("")(_.value))

  test("MOS generates the same science sequence as the equivalent long slit"):
    // 3 cycles = 60 minutes of science, short of either cadence.
    val ls = generateLongSlit(3)
    val ms = generateMos(3)

    assert(ls.nonEmpty, "expected a non-empty long slit sequence")
    assertEquals(ms.map(normalized), ls.map(normalized))

  test("100 minutes of science: long slit calibrates mid-block, MOS does not"):
    // 5 cycles at 20 minutes each, past the 90 minute cadence but short of 2 hours.
    val cycles = 5
    assert(CycleEstimate *| cycles === 100.minuteTimeSpan)

    assertEquals(
      titles(generateLongSlit(cycles)),
      List("ABBA Cycle", "ABBA Cycle", "ABBA Cycle", "Nighttime Calibrations", "ABBA Cycle", "ABBA Cycle", "Nighttime Calibrations")
    )

    assertEquals(
      titles(generateMos(cycles)),
      List("ABBA Cycle", "ABBA Cycle", "ABBA Cycle", "ABBA Cycle", "ABBA Cycle", "Nighttime Calibrations")
    )

  test("past 2 hours of science, MOS calibrates mid-block too"):
    // 7 cycles = 140 minutes, past the MOS cadence.
    val cycles = 7
    assert(CycleEstimate *| cycles === 140.minuteTimeSpan)

    assertEquals(
      titles(generateMos(cycles)),
      List("ABBA Cycle", "ABBA Cycle", "ABBA Cycle", "ABBA Cycle", "Nighttime Calibrations", "ABBA Cycle", "ABBA Cycle", "ABBA Cycle", "Nighttime Calibrations")
    )
