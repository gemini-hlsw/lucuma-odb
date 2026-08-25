// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.mos

import cats.syntax.option.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.TelluricType
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.flamingos2.Static
import lucuma.odb.sequence.flamingos2.spectroscopy.AcquisitionConfig
import lucuma.odb.sequence.flamingos2.spectroscopy.Config.Common
import lucuma.refined.*
import munit.FunSuite

import java.util.UUID

class AcquisitionSuite extends FunSuite:

  private val SlitWidth: Flamingos2CustomSlitWidth = Flamingos2CustomSlitWidth.CustomWidth_2_pix
  private val Disperser: Flamingos2Disperser       = Flamingos2Disperser.R1200HK
  private val Filter:    Flamingos2Filter          = Flamingos2Filter.HK
  private val AcqFilter: Flamingos2Filter          = Flamingos2Filter.H

  private val Namespace: UUID           = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val Oid:       Observation.Id = Observation.Id.fromLong(1L).get

  private val ExposureTime: TimeSpan = 30.secondTimeSpan

  private val CustomMask: Flamingos2FpuMask.Custom =
    Flamingos2FpuMask.Custom(ToBeDefined, SlitWidth)

  private def offsetQ(arcsec: Int): TelescopeConfig =
    TelescopeConfig.Default.copy(offset = Offset(Offset.P.Zero, Offset.Q(Angle.fromDoubleArcseconds(arcsec.toDouble))))

  private val common: Common =
    Common(
      exposureTimeMode    = ExposureTimeMode.SignalToNoiseMode(SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(100)), Wavelength.fromIntNanometers(2000).get),
      explicitReadMode    = none,
      explicitReads       = none,
      explicitDecker      = none,
      defaultReadoutMode  = Flamingos2ReadoutMode.Science,
      explicitReadoutMode = none,
      telescopeConfigs    = cats.data.NonEmptyList.of(offsetQ(15), offsetQ(-15), offsetQ(-15), offsetQ(15)),
      telluricType        = TelluricType.Hot
    )

  private def acqEtm(time: TimeSpan): ExposureTimeMode =
    ExposureTimeMode.TimeAndCountMode(time, 1.refined, Wavelength.fromIntNanometers(2000).get)

  private def config(
    explicitFilter: Option[Flamingos2Filter] = none,
    exposureTime:   TimeSpan                 = ExposureTime
  ): Config =
    Config(
      Disperser,
      Filter,
      CustomMask,
      AcquisitionConfig(acqEtm(exposureTime), AcqFilter, explicitFilter),
      common
    ).fold(m => fail(s"could not build the MOS config: $m"), identity)

  private val estimator: StepTimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig] =
    new StepTimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig]:
      override def estimateStep(
        static: Flamingos2StaticConfig,
        last:   StepTimeEstimateCalculator.Last[Flamingos2DynamicConfig],
        next:   ProtoStep[Flamingos2DynamicConfig]
      ): StepEstimate = StepEstimate.Zero

  private def generate(cfg: Config = config()): List[Atom[Flamingos2DynamicConfig]] =
    Acquisition
      .instantiate(Oid, estimator, Static, Namespace, cfg)
      .fold(e => fail(s"could not generate: $e"), _.generate.toList)

  private def qArcsec(s: Step[Flamingos2DynamicConfig]): Double =
    Angle.signedDecimalArcseconds.get(s.telescopeConfig.offset.q.toAngle).toDouble

  private def titles(as: List[Atom[Flamingos2DynamicConfig]]): List[String] =
    as.map(_.description.fold("")(_.value))

  test("the initial atom is mask out, sky nod, then the through-mask pair"):
    val steps = generate().head.steps.toList

    assertEquals(steps.length, 4)
    assertEquals(steps.map(_.instrumentConfig.fpu), List[Flamingos2FpuMask](Flamingos2FpuMask.Imaging, Flamingos2FpuMask.Imaging, CustomMask, CustomMask))
    assertEquals(steps.map(qArcsec), List(0.0, 10.0, 0.0, 10.0))
    assert(steps.forall(s => qArcsec(s) == 0.0 || qArcsec(s) == 10.0))
    assertEquals(steps.map(_.telescopeConfig.offset.p.toAngle.toMicroarcseconds).toSet, Set(0L))
    assertEquals(steps.map(_.observeClass).toSet, Set(ObserveClass.Acquisition))

  test("the breakpoint sits on the first mask-in step, so execution halts after step 2"):
    val steps = generate().head.steps.toList

    assertEquals(steps.map(_.breakpoint), List(Breakpoint.Disabled, Breakpoint.Disabled, Breakpoint.Enabled, Breakpoint.Disabled))

  test("the decker follows the aperture"):
    val steps = generate().head.steps.toList

    assertEquals(steps.map(_.instrumentConfig.decker), List(Flamingos2Decker.Imaging, Flamingos2Decker.Imaging, Flamingos2Decker.MOS, Flamingos2Decker.MOS))

  test("every step takes the acquisition exposure, the acquisition filter and no disperser"):
    val steps = generate().head.steps.toList

    assertEquals(steps.map(_.instrumentConfig.exposure).toSet, Set(ExposureTime))
    assertEquals(steps.map(_.instrumentConfig.filter).toSet, Set(AcqFilter))
    assertEquals(steps.map(_.instrumentConfig.disperser).toSet, Set(none[Flamingos2Disperser]))
    assertEquals(steps.map(_.instrumentConfig.readMode).toSet, Set(Flamingos2ReadMode.forExposureTime(ExposureTime)))

  test("an explicit filter overrides the default"):
    val steps = generate(config(Flamingos2Filter.KShort.some)).head.steps.toList

    assertEquals(steps.map(_.instrumentConfig.filter).toSet, Set(Flamingos2Filter.KShort))

  test("the initial atom is followed by breakpointed Fine Adjustments pairs"):
    val atoms = generate()

    assertEquals(atoms.length, Acquisition.RepeatingAtomCount + 1)
    assertEquals(titles(atoms).distinct, List("Initial Acquisition", "Fine Adjustments"))

    atoms.tail.foreach: a =>
      val ss = a.steps.toList
      assertEquals(ss.length, 2)
      assertEquals(ss.map(_.instrumentConfig.fpu), List[Flamingos2FpuMask](CustomMask, CustomMask))
      assertEquals(ss.map(qArcsec), List(0.0, 10.0))
      assertEquals(ss.map(_.breakpoint), List(Breakpoint.Enabled, Breakpoint.Disabled))

  test("a short exposure still takes both sky nods"):
    val steps = generate(config(exposureTime = 1.secondTimeSpan)).head.steps.toList

    assertEquals(steps.length, 4)
    assertEquals(steps.map(qArcsec), List(0.0, 10.0, 0.0, 10.0))

  test("a zero exposure time is an error"):
    assert(
      Acquisition
        .instantiate(Oid, estimator, Static, Namespace, config(exposureTime = TimeSpan.Zero))
        .isLeft
    )

  test("a signal-to-noise exposure time mode is an error"):
    val cfg =
      Config(
        Disperser,
        Filter,
        CustomMask,
        AcquisitionConfig(common.exposureTimeMode, AcqFilter, none),
        common
      ).fold(m => fail(s"could not build the MOS config: $m"), identity)

    assert(
      Acquisition
        .instantiate(Oid, estimator, Static, Namespace, cfg)
        .isLeft
    )
