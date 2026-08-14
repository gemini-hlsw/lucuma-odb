// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

/**
 * This is a unit test mostly to ensure all possible combination of params can be parsed by the
 * legacy ITC (Note that the ITC may still return an error but we want to ensure it can parse the
 * values
 */
class LegacyITCFlamingos2SpecSignalToNoiseSuite extends LegacyITCFlamingos2Suite:
  override def analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(5)

  override def obs = ItcObservationDetails(
    calculationMethod =
      ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.SpectroscopyIntegrationTime(
        sigma = 100,
        wavelengthAt = Wavelength.decimalNanometers.getOption(1200).get,
        coadds = None,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
    analysisMethod = analysisMethod
  )

  lazy val f2 =
    ObservingMode.SpectroscopyMode.Flamingos2(
      Flamingos2Disperser.R3000,
      Flamingos2Filter.J,
      Flamingos2ReadMode.Faint,
      Flamingos2FpuMask.Builtin(Flamingos2Fpu.LongSlit2),
      PortDisposition.Side
    )

  override def instrument = ItcInstrumentDetails(f2)

  override def title = "Flamingos2 Spectroscopy S/N"

  def observingModeWithFilter(f: Flamingos2Filter): ObservingMode =
    val d = f match
      case Flamingos2Filter.J | Flamingos2Filter.H | Flamingos2Filter.JH | Flamingos2Filter.HK =>
        Flamingos2Disperser.R1200JH
      case _                                                                                   => Flamingos2Disperser.R3000
    f2.copy(filter = f, disperser = d)

  def observingModeWithFpu(f: Flamingos2Fpu): ObservingMode =
    f2.copy(fpu = Flamingos2FpuMask.Builtin(f))

  def observingModeWithCustomMask(w: Flamingos2CustomSlitWidth): ObservingMode =
    f2.copy(fpu = Flamingos2FpuMask.Custom(ToBeDefined, w))

  def observingModeWithReadMode(rm: Flamingos2ReadMode): ObservingMode =
    f2.copy(readMode = rm)

  private def calculate(mode: ObservingMode) =
    localItc.calculate(bodyConf(sourceDefinition, obs, mode, analysisMethod).asJson.noSpaces)

  test(s"$title - Flamingos2 custom mask matches the equivalent longslit".tag(LegacyITCTest)):
    Enumerated[Flamingos2CustomSlitWidth].all
      .flatMap(w => w.fpu.tupleLeft(w))
      .traverse_ : (w, fpu) =>
        (calculate(observingModeWithCustomMask(w)), calculate(observingModeWithFpu(fpu))).tupled
          .map: (custom, builtin) =>
            assertEquals(custom, builtin, s"$w should match $fpu")
