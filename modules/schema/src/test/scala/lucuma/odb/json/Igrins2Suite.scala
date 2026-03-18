// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2StaticConfig
import munit.DisciplineSuite

import ArbIgrins2DynamicConfig.given
import ArbIgrins2StaticConfig.given
import igrins2.given
import time.query.given

class Igrins2Suite extends DisciplineSuite with ArbitraryInstances:

  checkAll("Igrins2Codec DynamicConfig", CodecTests[Igrins2DynamicConfig].codec)
  checkAll("Igrins2Codec StaticConfig",  CodecTests[Igrins2StaticConfig].codec)
