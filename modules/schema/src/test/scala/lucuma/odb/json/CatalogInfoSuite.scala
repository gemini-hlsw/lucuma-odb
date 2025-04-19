// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.CatalogInfo
import lucuma.core.model.arb.ArbCatalogInfo
import munit.DisciplineSuite

class CatalogInfoSuite extends DisciplineSuite with ArbitraryInstances {
  import ArbCatalogInfo.given
  import cataloginfo.given

  checkAll("CatalogInfoCodec", CodecTests[CatalogInfo].codec)
}
