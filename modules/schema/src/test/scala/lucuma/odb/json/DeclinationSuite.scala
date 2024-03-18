// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.Declination
import lucuma.core.math.arb.ArbDeclination
import munit.DisciplineSuite

abstract class DeclinationSuite(using Encoder[Declination]) extends DisciplineSuite with ArbitraryInstances {

  import ArbDeclination.given
  import declination.decoder.given

  checkAll("DeclinationCodec", CodecTests[Declination].codec)

  test("all declination encoders produce the same declination") {
    conversionTest[Declination]()
  }
}

class DeclinationQuerySuite extends DeclinationSuite(using declination.query.Encoder_Declination) {}

class DeclinationTransportSuite extends DeclinationSuite(using declination.query.Encoder_Declination) {}
