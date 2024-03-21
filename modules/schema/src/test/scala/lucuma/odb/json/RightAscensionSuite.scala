// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.RightAscension
import lucuma.core.math.arb.ArbRightAscension
import munit.DisciplineSuite

abstract class RightAscensionSuite(using Encoder[RightAscension]) extends DisciplineSuite with ArbitraryInstances {

  import ArbRightAscension.given
  import rightascension.decoder.given

  checkAll("RightAscensionCodec", CodecTests[RightAscension].codec)
}

class RightAscensionQuerySuite extends RightAscensionSuite(using rightascension.query.Encoder_Right_Ascension) {

  import ArbRightAscension.given
  import rightascension.query.given

  test("all rightascension encoders produce the same rightascension") {
    conversionTest[RightAscension]()
  }
}

class RightAscensionTransportSuite extends RightAscensionSuite(using rightascension.transport.Encoder_Right_Ascension) {}
