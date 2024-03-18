// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.RadialVelocity
import lucuma.core.math.arb.ArbRadialVelocity
import munit.DisciplineSuite

abstract class RadialVelocitySuite(using Encoder[RadialVelocity]) extends DisciplineSuite with ArbitraryInstances {

  import ArbRadialVelocity.given
  import radialvelocity.decoder.given

  checkAll("RadialVelocityCodec", CodecTests[RadialVelocity].codec)

  test("all radial velocity encoders produce the same radial velocity") {
    conversionTest[RadialVelocity]()
  }
}

class RadialVelocityQuerySuite extends RadialVelocitySuite(using radialvelocity.query.Encoder_Radial_Velocity) {}

class RadialVelocityTransportSuite extends RadialVelocitySuite(using radialvelocity.query.Encoder_Radial_Velocity) {}
