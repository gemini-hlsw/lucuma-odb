// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package spectroscopy

import cats.Eval
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.eq.*
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
 * GNIRS calibrates on a cadence set by the central wavelength: every 90
 * minutes in the near infrared, every hour from 2.6 μm.  A visit runs for
 * twice the period and every segment closes with its flats and arcs.
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

  private val CycleEstimate: TimeSpan = ExposureTime *| 4

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

  test("near infrared: a 3 hour visit closes with a single set of calibrations"):
    // 9 cycles = 180 minutes, exactly twice the 90 minute period.
    assert(CycleEstimate *| 9 === 3.hourTimeSpan)
    assertEquals(titles(9, NearInfrared), List.fill(9)(Sci) :+ Cal)

  test("near infrared: past 3 hours a second segment starts"):
    assertEquals(titles(10, NearInfrared), (List.fill(9)(Sci) :+ Cal) ++ List(Sci, Cal))

  test("thermal infrared: the visit is 2 hours"):
    // 7 cycles = 140 minutes, past the 2 hour visit for the 1 hour period.
    assertEquals(titles(7, ThermalInfrared), (List.fill(6)(Sci) :+ Cal) ++ List(Sci, Cal))

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

  test("mixed wavelengths: the tightest period sets the visit length"):
    // Two wavelengths share a 2 hour visit, so each segment gets 1 hour (3 cycles).
    val Sci2200 = s"$Sci (2200 nm)"
    val Cal2200 = s"$Cal (2200 nm)"
    val Sci3500 = s"$Sci (3500 nm)"
    val Cal3500 = s"$Cal (3500 nm)"
    assertEquals(
      titles(4, NearInfrared, ThermalInfrared),
      List(Sci2200, Sci2200, Sci2200, Cal2200, Sci3500, Sci3500, Sci3500, Cal3500, Sci2200, Cal2200, Sci3500, Cal3500)
    )
