// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.ghost.arb.ArbGhostDetector
import lucuma.core.model.sequence.ghost.arb.ArbGhostDynamicConfig
import lucuma.core.model.sequence.ghost.arb.ArbGhostStaticConfig
import munit.DisciplineSuite

class GhostSuite extends DisciplineSuite with ArbitraryInstances:

  import ArbEnumerated.given
  import ArbGhostDetector.given
  import ArbGhostDynamicConfig.given
  import ArbGhostStaticConfig.given
  import ghost.given
  import time.query.given

  checkAll("GhostCodec StaticConfig",       CodecTests[GhostStaticConfig].codec)
  checkAll("GhostCodec GhostDetector",      CodecTests[GhostDetector].codec)
  checkAll("GhostCodec GhostDynamicConfig", CodecTests[GhostDynamicConfig].codec)