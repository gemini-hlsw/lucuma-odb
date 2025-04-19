// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.Parallax
import lucuma.core.math.arb.ArbParallax
import munit.DisciplineSuite

abstract class ParallaxSuite(using Encoder[Parallax]) extends DisciplineSuite with ArbitraryInstances {

  import ArbParallax.given
  import parallax.decoder.given

  checkAll("ParallaxCodec", CodecTests[Parallax].codec)

  test("all parallax encoders produce the same parallax") {
    conversionTest[Parallax]()
  }
}

class ParallaxQuerySuite extends ParallaxSuite(using parallax.query.Encoder_Parallax) {}

class ParallaxTransportSuite extends ParallaxSuite(using parallax.query.Encoder_Parallax) {}
