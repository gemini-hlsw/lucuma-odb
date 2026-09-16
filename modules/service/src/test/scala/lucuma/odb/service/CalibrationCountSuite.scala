// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.option.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2SlitOffsetPreset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.flamingos2.defaultSlitTelescopeConfigs
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.flamingos2.longslit
import lucuma.odb.sequence.flamingos2.spectroscopy.AcquisitionConfig
import munit.FunSuite

class CalibrationCountSuite extends FunSuite:

  private val Etm: ExposureTimeMode =
    ExposureTimeMode.SignalToNoiseMode(
      SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(100)),
      Wavelength.fromIntNanometers(2000).get
    )

  private def f2(telluricType: TelluricType): longslit.Config =
    longslit.Config(
      Flamingos2Disperser.R1200HK,
      Flamingos2Filter.HK,
      Flamingos2Fpu.LongSlit2,
      Etm,
      AcquisitionConfig(Etm, Flamingos2Filter.H, none),
      defaultSlitTelescopeConfigs(Flamingos2SlitOffsetPreset.Telluric).telescopeConfigs,
      telluricType = telluricType
    )

  private def count(role: Option[CalibrationRole], minutes: Long, telluricType: TelluricType = TelluricType.Hot): Int =
    ObsExtract.calibrationCount(f2(telluricType), role, TimeSpan.unsafeFromMicroseconds(minutes * 60_000_000L)).value

  test("science observation: one telluric up to the threshold, two beyond"):
    assertEquals(count(none, 60), 1)
    assertEquals(count(none, 90), 1)
    assertEquals(count(none, 91), 2)

  test("NoTelluric requires none"):
    assertEquals(count(none, 120, TelluricType.NoTelluric), 0)

  test("a calibration observation requires none, whatever its mode says"):
    assertEquals(count(CalibrationRole.Telluric.some, 120), 0)
    assertEquals(count(CalibrationRole.SpectroPhotometric.some, 120), 0)
