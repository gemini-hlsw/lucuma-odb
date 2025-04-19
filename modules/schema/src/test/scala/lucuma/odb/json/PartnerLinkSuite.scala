// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.PartnerLink
import lucuma.core.model.arb.ArbPartnerLink
import munit.DisciplineSuite

class PartnerLinkSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbPartnerLink.given
  import partnerlink.given

  checkAll("PartnerLinkCodec", CodecTests[PartnerLink].codec)
  
}