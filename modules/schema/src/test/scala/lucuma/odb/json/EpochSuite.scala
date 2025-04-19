// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.Epoch
import lucuma.core.math.arb.ArbEpoch
import munit.DisciplineSuite

class EpochSuite extends DisciplineSuite with ArbitraryInstances {
  import ArbEpoch.given
  import epoch.given

  checkAll("EpochCodec", CodecTests[Epoch].codec)
}
