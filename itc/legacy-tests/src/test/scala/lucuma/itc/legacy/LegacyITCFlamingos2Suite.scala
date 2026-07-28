// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails.AnalysisMethod
import lucuma.itc.service.ObservingMode
import munit.Tag

/**
 * This is a unit test for Flamingos2 imaging mode in the legacy ITC, ensuring all possible
 * combinations of parameters can be parsed. The ITC may still return an error but we want to ensure
 * it can parse the values.
 */

// tags to filter out unnecessary tests for img or spec mode
// https://scalameta.org/munit/docs/filtering.html#filter-tests-cases-based-on-a-dynamic-conditions
object F2FpuTest      extends Tag("F2FpuTest")
object F2ReadModeTest extends Tag("F2ReadModeTest")

trait LegacyITCFlamingos2Suite extends CommonITCLegacySuite:

  def analysisMethod: AnalysisMethod

  def observingModeWithFilter(f: Flamingos2Filter): ObservingMode

  def observingModeWithFpu(f: Flamingos2Fpu): ObservingMode

  def observingModeWithReadMode(rm: Flamingos2ReadMode): ObservingMode

  def title: String

  test(s"$title - Flamingos2 filter".tag(LegacyITCTest)):
    assertAllValid(Enumerated[Flamingos2Filter].all): f =>
      localItc
        .calculate(
          bodyConf(sourceDefinition,
                   obs,
                   observingModeWithFilter(f),
                   analysisMethod
          ).asJson.noSpaces
        )

  test(s"$title - Flamingos2 fpu".tag(LegacyITCTest).tag(F2FpuTest)):
    assertAllValid(Enumerated[Flamingos2Fpu].all): f =>
      localItc
        .calculate(
          bodyConf(sourceDefinition, obs, observingModeWithFpu(f)).asJson.noSpaces
        )

  test(s"$title - Flamingos2 read mode".tag(LegacyITCTest).tag(F2ReadModeTest)):
    assertAllValid(Enumerated[Flamingos2ReadMode].all): rm =>
      localItc
        .calculate(
          bodyConf(sourceDefinition,
                   obs,
                   observingModeWithReadMode(rm),
                   analysisMethod
          ).asJson.noSpaces
        )

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
