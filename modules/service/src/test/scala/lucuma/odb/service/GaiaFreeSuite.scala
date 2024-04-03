// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.enums.Band
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class GaiaFreeSuite extends ScalaCheckSuite {

  import ArbSourceProfile.given

  // For now, fail if the band or source profile contains a Gaia band
  private def containsGaia(
    sourceProfile: SourceProfile
  ): Boolean = {
    val IsGaia = Set[Band](Band.Gaia, Band.GaiaBP, Band.GaiaRP)
    return
      SourceProfile
        .integratedBrightnesses
        .exist(m => (m.keySet & IsGaia).nonEmpty)(sourceProfile) ||
      SourceProfile
        .surfaceBrightnesses
        .exist(m => (m.keySet & IsGaia).nonEmpty)(sourceProfile)
  }

  test("gaia free source profile") {
    forAll { (sp: SourceProfile) =>
      assert(!containsGaia(sp.gaiaFree))
    }
  }

}
