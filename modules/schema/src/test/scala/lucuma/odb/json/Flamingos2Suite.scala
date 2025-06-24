// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2StaticConfig
import munit.DisciplineSuite

class Flamingos2Suite extends DisciplineSuite with ArbitraryInstances:

  import ArbFlamingos2DynamicConfig.given
  import ArbFlamingos2StaticConfig.given
  import flamingos2.given
  import time.query.given
  import wavelength.query.given

  checkAll("Flamingos2Codec DynamicConfig", CodecTests[Flamingos2DynamicConfig].codec)
  checkAll("Flamingos2Codec StaticConfig",  CodecTests[Flamingos2StaticConfig].codec)