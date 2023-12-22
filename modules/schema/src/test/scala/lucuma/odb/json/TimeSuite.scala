// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.core.util.arb.ArbTimestampInterval
import munit.DisciplineSuite

abstract class TimeSuite(using Encoder[TimeSpan], Encoder[TimestampInterval]) extends DisciplineSuite with ArbitraryInstances {

  import ArbTimeSpan.given
  import ArbTimestampInterval.given
  import time.decoder.given

  checkAll("TimeSpanCodec", CodecTests[TimeSpan].codec)
  checkAll("TimestampInterval", CodecTests[TimestampInterval].codec)
}

class TimeQuerySuite extends TimeSuite(using
  time.query.Encoder_TimeSpan,
  time.query.Encoder_TimestampInterval
)

class TimeTransportSuite extends TimeSuite(using
  time.transport.Encoder_TimeSpan,
  time.transport.Encoder_TimestampInterval
)

class TimeSpec extends munit.ScalaCheckSuite {
  import org.scalacheck.Prop.forAll
  property("includes value in error") {
    forAll { (str: String) =>
      import io.circe.syntax.*
      time.query.given_Decoder_TimeSpan.decodeJson(str.asJson) match {
        case Left(e) =>
          assert(e.getMessage.contains(s"Could not parse duration value ${str.asJson}"), e)
        case _       => fail("Expected error")
      }
    }
  }
}
