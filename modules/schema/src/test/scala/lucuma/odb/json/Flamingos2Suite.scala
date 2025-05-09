// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.f2.F2DynamicConfig
import lucuma.core.model.sequence.f2.F2StaticConfig
import lucuma.core.model.sequence.f2.arb.ArbF2DynamicConfig
import lucuma.core.model.sequence.f2.arb.ArbF2StaticConfig
import munit.DisciplineSuite

class Flamingos2Suite extends DisciplineSuite with ArbitraryInstances:

  import ArbF2DynamicConfig.given
  import ArbF2StaticConfig.given
  import f2.given
  import time.query.given

  checkAll("Flamingos2Codec DynamicConfig", CodecTests[F2DynamicConfig].codec)
  checkAll("Flamingos2Codec StaticConfig",  CodecTests[F2StaticConfig].codec)