// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package spectroscopy

import cats.Eval
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import munit.FunSuite

import java.util.UUID

/**
 * GNIRS runs each central wavelength as one segment closed by its own flats
 * and arcs, one set per started calibration set interval of its science.  A science cycle must be shorter than its wavelength's calibration
 * set interval: 90 minutes below 2.6 μm, an hour from it.
 */
class ScienceCadenceSuite extends FunSuite:

  private val Namespace: UUID           = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val Oid:       Observation.Id = Observation.Id.fromLong(1L).get

  private val ExposureTime: TimeSpan = 5.minTimeSpan

  private val NearInfrared: Wavelength = Wavelength.fromIntNanometers(2200).get
  private val ThermalInfrared: Wavelength = Wavelength.fromIntNanometers(3500).get

  private def offsetQ(arcsec: Int): TelescopeConfig =
    TelescopeConfig(Offset(Offset.P.Zero, Offset.Q(Angle.fromDoubleArcseconds(arcsec.toDouble))), StepGuideState.Enabled)

  private val abbaOffsets: NonEmptyList[TelescopeConfig] =
    NonEmptyList.of(offsetQ(2), offsetQ(-4), offsetQ(-4), offsetQ(2))

  private def etm(w: Wavelength): ExposureTimeMode =
    ExposureTimeMode.SignalToNoiseMode(SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(100)), w)

  private def wavelength(w: Wavelength): CentralWavelengthConfig =
    CentralWavelengthConfig(w, etm(w), PosInt.unsafeFrom(1))

  private def config(ws: Wavelength*): Config =
    Config(
      filter           = GnirsFilter.K,
      decker           = GnirsDecker.ShortCamLongSlit,
      fpu              = GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
      prism            = GnirsPrism.Mirror,
      grating          = GnirsGrating.D32,
      wavelengths      = NonEmptyList.fromListUnsafe(ws.toList.map(wavelength)),
      camera           = GnirsCamera.ShortBlue,
      focus            = GnirsFocus.Best,
      explicitReadMode = none,
      wellDepth        = GnirsWellDepth.Shallow,
      telescopeConfigs = abbaOffsets,
      acquisition      = AcquisitionConfig(none, none, etm(ws.head), false, PosInt.unsafeFrom(1)),
      telluricType     = TelluricType.Hot
    )

  private val Static: GnirsStaticConfig = GnirsStaticConfig(GnirsWellDepth.Shallow)

  private val expander: SmartGcalExpander[Eval, GnirsStaticConfig, GnirsDynamicConfig] =
    SmartGcalExpander.pure[Eval, GnirsStaticConfig, GnirsDynamicConfig]: (_, _, d) =>
      (d, StepConfig.Gcal(StepConfig.Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W), GcalFilter.None, GcalDiffuser.Ir, GcalShutter.Open), ObserveClass.NightCal)

  // Each step costs its own exposure time, so a cycle costs 4 x ExposureTime.
  private val estimator: StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig] =
    new StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig]:
      override def estimateStep(
        static: GnirsStaticConfig,
        last:   StepTimeEstimateCalculator.Last[GnirsDynamicConfig],
        next:   ProtoStep[GnirsDynamicConfig]
      ): StepEstimate =
        StepEstimate.fromMax(List(ConfigChangeEstimate("test", "test", next.value.exposure)), Nil)

  private def generate(cycles: Int, ws: Wavelength*): Either[OdbError, List[Atom[GnirsDynamicConfig]]] =
    val times = NonEmptyList.fromListUnsafe(ws.toList).map(w => w -> IntegrationTime(ExposureTime, PosInt.unsafeFrom(cycles * 4)))
    Science
      .instantiate[Eval](Oid, estimator, Static, Namespace, expander, config(ws*), times.asRight, none)
      .value
      .map(_.generate.toList)

  private def titles(cycles: Int, ws: Wavelength*): List[String] =
    generate(cycles, ws*).fold(e => fail(s"could not generate: $e"), _.map(_.description.fold("")(_.value)))

  private val Sci = "Science Cycle"
  private val Cal = "Nighttime Calibrations"

  test("near infrared: one closing set per started 90 minutes of science"):
    // 20 minute cycles: 4 = 80 minutes, 9 = 180, 10 = 200.
    List(4 -> 1, 9 -> 2, 10 -> 3).foreach: (cycles, sets) =>
      assertEquals(titles(cycles, NearInfrared), List.fill(cycles)(Sci) ++ List.fill(sets)(Cal), s"$cycles cycles")

  test("thermal infrared: one closing set per started hour, exactly an hour takes one"):
    List(3 -> 1, 4 -> 2, 7 -> 3).foreach: (cycles, sets) =>
      assertEquals(titles(cycles, ThermalInfrared), List.fill(cycles)(Sci) ++ List.fill(sets)(Cal), s"$cycles cycles")

  test("thermal infrared: a cycle longer than an hour is rejected"):
    val long = Science
      .instantiate[Eval](
        Oid, estimator, Static, Namespace, expander, config(ThermalInfrared),
        NonEmptyList.one(ThermalInfrared -> IntegrationTime(20.minTimeSpan, PosInt.unsafeFrom(4))).asRight,
        none
      )
      .value
    assert(long.isLeft, "an 80 minute cycle should not fit the thermal infrared period")
    assert(generate(1, NearInfrared).isRight)

  test("a near infrared cycle may run past an hour beside a thermal infrared wavelength"):
    // 80 minute cycles at 2200 nm, 20 minute cycles at 3500 nm: each wavelength
    // is held to its own interval.
    val times = NonEmptyList.of(
      NearInfrared    -> IntegrationTime(20.minTimeSpan, PosInt.unsafeFrom(4)),
      ThermalInfrared -> IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))
    )
    val gen = Science
      .instantiate[Eval](Oid, estimator, Static, Namespace, expander, config(NearInfrared, ThermalInfrared), times.asRight, none)
      .value
    assert(gen.isRight, s"expected per wavelength limits, got $gen")

  test("mixed wavelengths: each runs contiguously and closes with its own calibrations"):
    // 80 minutes at each: one set at 2200 nm, two at 3500 nm.
    val Sci2200 = s"$Sci (2200 nm)"
    val Cal2200 = s"$Cal (2200 nm)"
    val Sci3500 = s"$Sci (3500 nm)"
    val Cal3500 = s"$Cal (3500 nm)"
    assertEquals(
      titles(4, NearInfrared, ThermalInfrared),
      List.fill(4)(Sci2200) ++ List(Cal2200) ++ List.fill(4)(Sci3500) ++ List(Cal3500, Cal3500)
    )
