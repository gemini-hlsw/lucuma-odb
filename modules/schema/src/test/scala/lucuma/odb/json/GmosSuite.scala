// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.arb.ArbDynamicConfig
import munit.DisciplineSuite

class GmosSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbDynamicConfig.*
  import angle.query.given
  import gmos.given
  import time.query.given
  import wavelength.query.given

  checkAll("GmosCodec", CodecTests[GmosNorth].codec)
}
