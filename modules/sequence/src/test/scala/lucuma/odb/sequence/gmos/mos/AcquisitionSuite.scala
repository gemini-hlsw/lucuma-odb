// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.mos

import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.enums.GmosMosAcquisitionType
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gmos.mos.DefaultTelescopeConfigs
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos.InitialConfigs
import lucuma.odb.sequence.gmos.spectroscopy.Config.Common
import munit.FunSuite

import java.util.UUID

class AcquisitionSuite extends FunSuite:

  private val SlitWidth: GmosCustomSlitWidth = GmosCustomSlitWidth.CustomWidth_1_00
  private val Grating:   GmosNorthGrating    = GmosNorthGrating.R831_G5302
  private val Filter:    GmosNorthFilter     = GmosNorthFilter.GPrime
  private val Count:     PosInt              = PosInt.unsafeFrom(10)

  private val Namespace: UUID           = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val Oid:       Observation.Id   = Observation.Id.fromLong(1L).get

  private def acqConfig(time: Int = 30): AcquisitionConfig.GmosNorth =
    AcquisitionConfig.GmosNorth(
      ExposureTimeMode.TimeAndCountMode(TimeSpan.unsafeFromMicroseconds(time.toLong * 1_000_000L), Count, Wavelength.fromIntNanometers(500).get),
      Filter,
      none
    )

  private val common: Common =
    Common(
      centralWavelength         = Wavelength.fromIntNanometers(500).get,
      exposureTimeMode          = ExposureTimeMode.TimeAndCountMode(300.secondTimeSpan, Count, Wavelength.fromIntNanometers(500).get),
      defaultXBin               = GmosXBinning.One,
      explicitXBin              = none,
      defaultYBin               = GmosYBinning.Two,
      explicitYBin              = none,
      explicitAmpReadMode       = none,
      explicitAmpGain           = none,
      explicitRoi               = none,
      explicitWavelengthDithers = none
    )

  private def config(
    acquisitionType: GmosMosAcquisitionType,
    acq:             AcquisitionConfig.GmosNorth = acqConfig()
  ): Config.GmosNorth =
    Config.GmosNorth(
      Grating,
      Filter.some,
      GmosFpuMask.Custom(ToBeDefined, SlitWidth),
      acquisitionType,
      acq,
      common,
      DefaultTelescopeConfigs
    )

  private val estimator: StepTimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
    new StepTimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]:
      override def estimateStep(
        static: StaticConfig.GmosNorth,
        last:   StepTimeEstimateCalculator.Last[DynamicConfig.GmosNorth],
        next:   ProtoStep[DynamicConfig.GmosNorth]
      ): StepEstimate = StepEstimate.Zero

  private def generate(
    cfg:     Config.GmosNorth,
    calRole: Option[CalibrationRole] = none
  ): List[Atom[DynamicConfig.GmosNorth]] =
    Acquisition
      .gmosNorth(Oid, estimator, InitialConfigs.GmosNorthStatic, Namespace, cfg, calRole)
      .fold(e => fail(s"could not generate: $e"), _.generate.toList)

  private def fpuOf(s: Step[DynamicConfig.GmosNorth]): Option[GmosFpuMask[GmosNorthFpu]] =
    s.instrumentConfig.fpu

  private val CustomMask: GmosFpuMask[GmosNorthFpu] =
    GmosFpuMask.Custom(ToBeDefined, SlitWidth)

  test("MaskIn: initial atom is a single through-mask step with a breakpoint, then count-1 repeats"):
    val atoms = generate(config(GmosMosAcquisitionType.MaskIn))

    assertEquals(atoms.length, Count.value)
    assertEquals(atoms.head.steps.length, 1)
    val initial = atoms.head.steps.head
    assertEquals(initial.breakpoint, Breakpoint.Enabled)
    assert(fpuOf(initial).contains(CustomMask), s"through-mask step carries the custom mask, got ${fpuOf(initial)}")

    val repeats = atoms.tail
    assert(repeats.nonEmpty)
    repeats.foreach: a =>
      assertEquals(a.steps.length, 1)
      assertEquals(a.steps.head.breakpoint, Breakpoint.Disabled)

  test("MaskOut: initial atom is a field step then a through-mask step with a breakpoint, then count-1 repeats"):
    val atoms = generate(config(GmosMosAcquisitionType.MaskOut))

    assertEquals(atoms.length, Count.value)
    assertEquals(atoms.head.steps.length, 2)
    val field  = atoms.head.steps.head
    val masked = atoms.head.steps.last
    assertEquals(fpuOf(field), none, "the field step is taken without the mask")
    assert(fpuOf(masked).contains(CustomMask), "the second step is taken through the mask")
    assertEquals(masked.breakpoint, Breakpoint.Enabled)
    assertEquals(field.breakpoint, Breakpoint.Disabled)

    val repeats = atoms.tail
    assert(repeats.nonEmpty)
    repeats.foreach: a =>
      assertEquals(a.steps.length, 1)
      assertEquals(a.steps.head.breakpoint, Breakpoint.Disabled)

  test("every step is Full Frame, carries the acquisition filter, no grating, and no offset"):
    val atoms = generate(config(GmosMosAcquisitionType.MaskOut))
    val steps = atoms.flatMap(_.steps.toList)

    assert(steps.nonEmpty)
    steps.foreach: s =>
      assertEquals(s.instrumentConfig.roi, GmosRoi.FullFrame, "ROI must be Full Frame")
      assertEquals(s.instrumentConfig.filter, Filter.some, "step carries the acquisition filter")
      assertEquals(s.instrumentConfig.gratingConfig, none, "no grating on acquisition steps")
      assertEquals(s.telescopeConfig.offset, Offset.Zero, "no offset on acquisition steps")
      assertEquals(s.observeClass, ObserveClass.Acquisition)

  test("through-mask steps are 1x1; the unmasked field step is 2x2"):
    val atoms = generate(config(GmosMosAcquisitionType.MaskOut))

    // Under MaskOut the first step of the initial atom is the field image and
    // every other step in the sequence is taken through the mask.
    val steps = atoms.flatMap(_.steps.toList)
    val field = steps.head
    val masked = steps.tail

    assertEquals(fpuOf(field), none)
    assertEquals(field.instrumentConfig.readout.xBin, GmosXBinning.Two)
    assertEquals(field.instrumentConfig.readout.yBin, GmosYBinning.Two)

    assert(masked.nonEmpty)
    masked.foreach: s =>
      assert(fpuOf(s).contains(CustomMask))
      assertEquals(s.instrumentConfig.readout.xBin, GmosXBinning.One)
      assertEquals(s.instrumentConfig.readout.yBin, GmosYBinning.One)

  test("exposure time is the stated time, verbatim"):
    val atoms = generate(config(GmosMosAcquisitionType.MaskIn, acqConfig(45)))
    val steps = atoms.flatMap(_.steps.toList)
    steps.foreach(s => assertEquals(s.instrumentConfig.exposure, 45.secondTimeSpan))

  test("read mode is Fast at or below 60 seconds and Slow above"):
    val fast = generate(config(GmosMosAcquisitionType.MaskIn, acqConfig(60)))
    fast.flatMap(_.steps.toList).foreach(s => assertEquals(s.instrumentConfig.readout.ampReadMode, GmosAmpReadMode.Fast))

    val slow = generate(config(GmosMosAcquisitionType.MaskIn, acqConfig(61)))
    slow.flatMap(_.steps.toList).foreach(s => assertEquals(s.instrumentConfig.readout.ampReadMode, GmosAmpReadMode.Slow))

  test("an acquisition count of 1 produces no repeat atoms"):
    val acq1 = acqConfig().copy(
      exposureTimeMode = ExposureTimeMode.TimeAndCountMode(30.secondTimeSpan, PosInt.unsafeFrom(1), Wavelength.fromIntNanometers(500).get)
    )
    val atoms = generate(config(GmosMosAcquisitionType.MaskOut, acq1))
    assertEquals(atoms.length, 1)
    assertEquals(atoms.head.steps.length, 2)

  test("a Twilight calibration generates an empty acquisition"):
    val atoms = generate(config(GmosMosAcquisitionType.MaskIn), calRole = CalibrationRole.Twilight.some)
    assertEquals(atoms.length, 0)

  test("a signal-to-noise acquisition exposure time mode is rejected"):
    val snConfig = config(
      GmosMosAcquisitionType.MaskIn,
      acqConfig().copy(
        exposureTimeMode = ExposureTimeMode.SignalToNoiseMode(
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(10)),
          Wavelength.fromIntNanometers(500).get
        )
      )
    )
    val result = Acquisition.gmosNorth(Oid, estimator, InitialConfigs.GmosNorthStatic, Namespace, snConfig, none)
    assert(result.isLeft, "expected a signal-to-noise acquisition ETM to be rejected")

end AcquisitionSuite
