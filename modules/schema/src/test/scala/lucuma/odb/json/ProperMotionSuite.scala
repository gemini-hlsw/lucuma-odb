// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.ProperMotion
import lucuma.core.math.arb.ArbProperMotion.given
import munit.DisciplineSuite

abstract class ProperMotionSuite(using Encoder[ProperMotion]) extends DisciplineSuite with ArbitraryInstances {
  import propermotion.decoder.given

  checkAll("ProperMotionCodec", CodecTests[ProperMotion].codec)
}

class ProperMotionQuerySuite extends ProperMotionSuite(using propermotion.query.Encoder_ProperMotion) {}

class ProperMotionTransportSuite extends ProperMotionSuite(using propermotion.query.Encoder_ProperMotion) {}

abstract class ProperMotionRASuite(using Encoder[ProperMotion.RA]) extends DisciplineSuite with ArbitraryInstances {
  import propermotion.decoder.given

  checkAll("ProperMotionRACodec", CodecTests[ProperMotion.RA].codec)

  test("all propermotion.ra encoders produce the same propermotion.ra") {
    conversionTest[ProperMotion.RA]()
  }
}

class ProperMotionRAQuerySuite extends ProperMotionRASuite(using propermotion.query.Encoder_ProperMotion_RA) {}

class ProperMotionRATransportSuite extends ProperMotionRASuite(using propermotion.query.Encoder_ProperMotion_RA) {}

abstract class ProperMotionDecSuite(using Encoder[ProperMotion.Dec]) extends DisciplineSuite with ArbitraryInstances {
  import propermotion.decoder.given

  checkAll("ProperMotionDecCodec", CodecTests[ProperMotion.Dec].codec)

  test("all propermotion.dec encoders produce the same propermotion.dec") {
    conversionTest[ProperMotion.Dec]()
  }
}

class ProperMotionDecQuerySuite extends ProperMotionDecSuite(using propermotion.query.Encoder_ProperMotion_Dec) {}

class ProperMotionDecTransportSuite extends ProperMotionDecSuite(using propermotion.query.Encoder_ProperMotion_Dec) {}
