// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.format

import cats.data.NonEmptyList
import cats.syntax.option.*
import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.model.sequence.TelescopeConfigSpec
import lucuma.core.model.sequence.arb.ArbSlitTelescopeConfigs
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import telescopeConfigs.*

class TelescopeConfigsFormatSuite extends ScalaCheckSuite:
  import ArbSlitTelescopeConfigs.given

  property("SlitTelescopeConfigsFormat round trips"):
    forAll: (stc: SlitTelescopeConfigs) =>
      assertEquals(SlitTelescopeConfigsFormat.getOption(SlitTelescopeConfigsFormat.reverseGet(stc)), stc.some)

  private val alongSlit: NonEmptyList[TelescopeConfigAlongSlit] =
    NonEmptyList.of(
      TelescopeConfigAlongSlit(-(1.25.qArcsec), StepGuideState.Enabled),
      TelescopeConfigAlongSlit(  1.25.qArcsec,  StepGuideState.Enabled)
    )

  private val toSky: NonEmptyList[TelescopeConfigSpec] =
    NonEmptyList.of(
      TelescopeConfigSpec(Offset.Zero, StepGuideState.Enabled),
      TelescopeConfigSpec(Offset(10.pArcsec, 10.qArcsec), StepGuideState.Disabled)
    )

  test("a pattern without explicit purposes is stored as before purposes existed"):
    assert(!AlongSlitFormat.reverseGet(alongSlit).contains("explicitPurpose"))
    assert(!ToSkyFormat.reverseGet(toSky).contains("explicitPurpose"))

  // As written by earlier migrations, before purposes existed.
  test("a pattern stored before purposes existed decodes with none specified"):
    val legacyAlongSlit = """[{"q":{"microarcseconds":-1250000},"guiding":"ENABLED"},{"q":{"microarcseconds":1250000},"guiding":"ENABLED"}]"""
    val legacyToSky     = """[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":0}},"guiding":"ENABLED"},{"offset":{"p":{"microarcseconds":10000000},"q":{"microarcseconds":10000000}},"guiding":"DISABLED"}]"""
    assertEquals(AlongSlitFormat.getOption(legacyAlongSlit), alongSlit.some)
    assertEquals(ToSkyFormat.getOption(legacyToSky), toSky.some)

  test("an explicit purpose is stored and read back"):
    val withSky = toSky.map(_.copy(explicitPurpose = OffsetPurpose.Sky.some))
    assertEquals(ToSkyFormat.getOption(ToSkyFormat.reverseGet(withSky)), withSky.some)
