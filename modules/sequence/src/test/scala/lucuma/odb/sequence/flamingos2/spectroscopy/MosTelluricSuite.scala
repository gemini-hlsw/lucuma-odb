// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.spectroscopy

import cats.Eval
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2SlitOffsetPreset
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.flamingos2.defaultSlitTelescopeConfigs
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.flamingos2.longslit
import munit.FunSuite

import java.util.UUID

/**
 * The telluric standard of a Flamingos 2 MOS observation, which is a long slit
 * observation stepping the star down the slit rather than nodding ABBA.
 */
class MosTelluricSuite extends FunSuite:

  private val EquivalentFpu: Flamingos2Fpu       = Flamingos2Fpu.LongSlit2
  private val Disperser:     Flamingos2Disperser = Flamingos2Disperser.R1200HK
  private val Filter:        Flamingos2Filter    = Flamingos2Filter.HK
  private val AcqFilter:     Flamingos2Filter    = Flamingos2Filter.H

  private val Namespace: UUID           = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val Oid:       Observation.Id = Observation.Id.fromLong(1L).get

  private val ExposureTime: TimeSpan = 5.minTimeSpan

  private val At: Wavelength = Wavelength.fromIntNanometers(2000).get

  private val ScienceEtm: ExposureTimeMode =
    ExposureTimeMode.SignalToNoiseMode(SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(100)), At)

  private def config(
    telescopeConfigs:    NonEmptyList[TelescopeConfig] = longslit.Config.MosTelluricTelescopeConfigs.telescopeConfigs,
    telluricScienceMode: Option[ObservingModeType]     = ObservingModeType.Flamingos2Mos.some
  ): longslit.Config =
    longslit.Config(
      Disperser,
      Filter,
      EquivalentFpu,
      ScienceEtm,
      AcquisitionConfig(ScienceEtm, AcqFilter, none),
      telescopeConfigs,
      telluricScienceMode = telluricScienceMode
    )

  private val expander: SmartGcalExpander[Eval, Flamingos2StaticConfig, Flamingos2DynamicConfig] =
    SmartGcalExpander.pure[Eval, Flamingos2StaticConfig, Flamingos2DynamicConfig]: (_, _, d) =>
      (d, StepConfig.Gcal(StepConfig.Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W), GcalFilter.None, GcalDiffuser.Ir, GcalShutter.Open), ObserveClass.NightCal)

  private val estimator: StepTimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig] =
    new StepTimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig]:
      override def estimateStep(
        static: Flamingos2StaticConfig,
        last:   StepTimeEstimateCalculator.Last[Flamingos2DynamicConfig],
        next:   ProtoStep[Flamingos2DynamicConfig]
      ): StepEstimate =
        StepEstimate.fromMax(List(ConfigChangeEstimate("test", "test", next.value.exposure)), Nil)

  private def itc(exposures: Int): Either[OdbError, IntegrationTime] =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(exposures)).asRight

  private def generate(
    exposures: Int                     = 6,
    calRole:   Option[CalibrationRole] = CalibrationRole.Telluric.some,
    cfg:       longslit.Config         = config()
  ): StreamingExecutionConfig[fs2.Pure, Flamingos2StaticConfig, Flamingos2DynamicConfig] =
    longslit.LongSlit
      .instantiate[Eval](Oid, estimator, Namespace, expander, cfg, itc(exposures), itc(exposures), calRole)
      .value
      .fold(e => fail(s"could not generate: $e"), identity)

  private def qArcsec(s: Step[Flamingos2DynamicConfig]): Double =
    Angle.signedDecimalArcseconds.get(s.telescopeConfig.offset.q.toAngle).toDouble

  private def titles(as: List[Atom[Flamingos2DynamicConfig]]): List[String] =
    as.map(_.description.fold("")(_.value))

  test("the science sequence is one telluric atom followed by an arc"):
    val atoms = generate().science.toList

    assertEquals(titles(atoms), List("Telluric", "Nighttime Calibrations"))
    assertEquals(atoms.map(_.steps.length), List(6, 1))

  test("the standard is stepped down the slit at p = 0"):
    val steps = generate().science.toList.head.steps.toList

    assertEquals(steps.map(qArcsec), List(60.0, 40.0, 20.0, -20.0, 40.0, 60.0))
    assertEquals(steps.map(_.telescopeConfig.offset.p.toAngle.toMicroarcseconds).toSet, Set(0L))
    assertEquals(steps.map(_.telescopeConfig.guiding).toSet, Set(StepGuideState.Enabled))

  test("science steps are night calibrations, and so is the arc"):
    val atoms = generate().science.toList

    assertEquals(atoms.head.steps.toList.map(_.observeClass).toSet, Set(ObserveClass.NightCal))
    assertEquals(atoms.last.steps.head.observeClass, ObserveClass.NightCal)

  test("the arc is taken unguided at the last science offset"):
    val arc = generate().science.toList.last.steps.head

    assertEquals(qArcsec(arc), 60.0)
    assertEquals(arc.telescopeConfig.guiding, StepGuideState.Disabled)
    assert(arc.stepConfig.isInstanceOf[StepConfig.Gcal])

  test("the arc closes the sequence however many cycles the ITC asks for"):
    // 13 exposures over a 6 position pattern rounds up to three cycles.
    val atoms = generate(exposures = 13).science.toList

    assertEquals(titles(atoms), List("Telluric", "Telluric", "Telluric", "Nighttime Calibrations"))

  test("a long slit observation's own telluric keeps the ABBA cadence"):
    val atoms = generate(cfg = config(defaultSlitTelescopeConfigs(Flamingos2SlitOffsetPreset.Telluric).telescopeConfigs
, none)).science.toList

    assertEquals(titles(atoms).distinct, List("ABBA Cycle", "Nighttime Calibrations"))
