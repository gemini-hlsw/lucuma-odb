// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle
import munit.DisciplineSuite

abstract class AngleSuite(using Encoder[Angle]) extends DisciplineSuite with ArbitraryInstances {

  import ArbAngle.given
  import angle.decoder.given

  checkAll("AngleCodec", CodecTests[Angle].codec)

  val angleKeys: Set[String] =
    Set(
      "microarcseconds",
      "milliarcseconds",
      "arcseconds",
      "arcminutes",
      "degrees",
      "dms"
    )

  test("all `angle` angle encoders produce the same angle") {
    conversionTest[Angle](angleKeys)
  }

}

class AngleQuerySuite extends AngleSuite(using
  angle.query.Encoder_Angle
) {

  import ArbAngle.given

  val timeKeys: Set[String] =
    Set(
      "microseconds",
      "milliseconds",
      "seconds",
      "minutes",
      "hours",
      "hms"
    )

  import angle.query.given

  test("all `time` angle encoders produce the same angle") {
    conversionTest[Angle](timeKeys)
  }

}

class AngleTransportSuite extends AngleSuite(using
  angle.transport.Encoder_Angle
)
