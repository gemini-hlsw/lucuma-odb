// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.implicits.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

/**
 * This is a unit test for GMOS imaging mode in the legacy ITC, ensuring all possible combinations
 * of parameters can be parsed. The ITC may still return an error but we want to ensure it can parse
 * the values.
 */
class LegacyITCGmosImgSignalToNoiseSuite extends CommonITCLegacySuite:

  // Define observation details with signal-to-noise calculation method
  override val obs = ItcObservationDetails(
    calculationMethod =
      ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.ImagingIntegrationTime(
        sigma = 600,
        coadds = None,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
    analysisMethod = lsAnalysisMethod
  )

  // Define GMOS imaging TxC instrument
  override val instrument = ItcInstrumentDetails(
    ObservingMode.ImagingMode.GmosNorth(
      GmosNorthFilter.GPrime,
      GmosCcdMode(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.High,
        GmosAmpReadMode.Fast
      ).some
    )
  )

  // Testing observing conditions
  testConditions("GMOS imaging TxC S/N", baseParams)

  // GMOS North filter testing
  val gmosNConf = ObservingMode.ImagingMode.GmosNorth(
    GmosNorthFilter.GPrime,
    GmosCcdMode(
      GmosXBinning.One,
      GmosYBinning.One,
      GmosAmpCount.Twelve,
      GmosAmpGain.High,
      GmosAmpReadMode.Fast
    ).some
  )

  test("gmos north filter".tag(LegacyITCTest)):
    Enumerated[GmosNorthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, gmosNConf.copy(filter = f)).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, containsValidResults))

  // GMOS South filter testing
  val gmosSConf = ObservingMode.ImagingMode.GmosSouth(
    GmosSouthFilter.GPrime,
    GmosCcdMode(
      GmosXBinning.One,
      GmosYBinning.One,
      GmosAmpCount.Twelve,
      GmosAmpGain.High,
      GmosAmpReadMode.Fast
    ).some
  )

  test("gmos south filter".tag(LegacyITCTest)):
    Enumerated[GmosSouthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, gmosSConf.copy(filter = f)).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, containsValidResults))

  // Testing various SEDs
  testSEDs("GMOS imaging TxC S/N", baseParams, false, false)

  // Testing user defined SED
  testUserDefinedSED("GMOS imaging TxC S/N", baseParams)

  // Testing brightness units
  testBrightnessUnits("GMOS imaging TxC S/N", baseParams)

  // Testing power law and blackbody
  testPowerAndBlackbody("GMOS imaging TxC S/N", baseParams)
