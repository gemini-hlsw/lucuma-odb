// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.instances

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbSourceProfile
import munit.DisciplineSuite

class SourceProfileCodecSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbSourceProfile.given
  import SourceProfileCodec.given

  checkAll("SourceProfileCodec", CodecTests[SourceProfile].codec)

}
