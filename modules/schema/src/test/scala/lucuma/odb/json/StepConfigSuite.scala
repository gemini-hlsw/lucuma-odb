// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.arb.ArbStepConfig
import munit.DisciplineSuite

class StepConfigSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbStepConfig.given

  import offset.query.given
  import stepconfig.given

  checkAll("StepConfigCodec", CodecTests[StepConfig].codec)
}
