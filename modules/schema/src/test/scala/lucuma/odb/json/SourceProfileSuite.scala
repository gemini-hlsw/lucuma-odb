// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbSourceProfile
import munit.DisciplineSuite

class SourceProfileSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbSourceProfile.given
  import angle.query.given
  import sourceprofile.given
  import wavelength.query.given

  checkAll("SourceProfileCodec", CodecTests[SourceProfile].codec)

}
