// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails.AnalysisMethod
import lucuma.itc.service.ObservingMode

/**
 * This is a unit test for Flamingos2 imaging mode in the legacy ITC, ensuring all possible
 * combinations of parameters can be parsed. The ITC may still return an error but we want to ensure
 * it can parse the values.
 */
trait LegacyITCFlamingos2Suite extends CommonITCLegacySuite:

  def analysisMethod: AnalysisMethod

  def observingModeWithFilter(f: Flamingos2Filter): ObservingMode

  def observingModeWithFpu(f: Flamingos2Fpu): ObservingMode

  def title: String

  test(s"$title - Flamingos2 filter".tag(LegacyITCTest)):
    Enumerated[Flamingos2Filter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition,
                   obs,
                   observingModeWithFilter(f),
                   analysisMethod
          ).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test(s"$title - Flamingos2 fpu".tag(LegacyITCTest)):
    Enumerated[Flamingos2Fpu].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, observingModeWithFpu(f)).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  // Testing observing conditions
  testConditions(title, baseParams)

  // Testing various SEDs
  testSEDs(title, baseParams)

  // Testing user defined SED
  testUserDefinedSED(title, baseParams)

  // Testing brightness units
  testBrightnessUnits(title, baseParams)

  // Testing power law and blackbody
  testPowerAndBlackbody(title, baseParams)
