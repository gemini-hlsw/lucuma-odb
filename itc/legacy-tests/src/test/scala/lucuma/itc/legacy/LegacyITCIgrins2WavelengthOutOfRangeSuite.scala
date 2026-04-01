// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.syntax.*
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

class LegacyITCIgrins2WavelengthOutOfRangeSuite extends CommonITCLegacySuite:

  val wavelengthAt = Wavelength.decimalMicrometers.getOption(1.9).get

  override def obs = ItcObservationDetails(
    calculationMethod =
      ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.SpectroscopyIntegrationTime(
        sigma = 2,
        coadds = None,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0,
        wavelengthAt = wavelengthAt
      ),
    analysisMethod = lsAnalysisMethod
  )

  override def instrument = ItcInstrumentDetails(
    ObservingMode.SpectroscopyMode.Igrins2(PortDisposition.Bottom)
  )

  test("igrins2 at 1.9 µm returns out-of-range".tag(LegacyITCTest)):
    val result = localItc
      .calculate(baseParams.asJson.noSpaces)
    assertIO(
      result.map:
        case Left(msgs) => msgs.contains(LocalItc.OutOfRangeMsg)
        case Right(_)   => false
      ,
      true
    )
