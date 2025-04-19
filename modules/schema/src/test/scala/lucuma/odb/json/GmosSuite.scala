// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig
import lucuma.core.model.sequence.gmos.arb.ArbStaticConfig
import munit.DisciplineSuite

class GmosSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbDynamicConfig.given
  import ArbStaticConfig.given
  import gmos.given
  import offset.query.given
  import time.query.given
  import wavelength.query.given

  checkAll("GmosCodec DynamicConfig GmosNorth", CodecTests[DynamicConfig.GmosNorth].codec)
  checkAll("GmosCodec DynamicConfig GmosSouth", CodecTests[DynamicConfig.GmosSouth].codec)
  checkAll("GmosCodec StaticConfig GmosNorth",  CodecTests[StaticConfig.GmosNorth].codec)
  checkAll("GmosCodec StaticConfig GmosSouth",  CodecTests[StaticConfig.GmosSouth].codec)
}
