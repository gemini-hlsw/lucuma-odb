// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.util.DateInterval
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.core.util.arb.ArbTimestampInterval
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.LocalDate

abstract class TimeSuite(using Encoder[DateInterval], Encoder[TimeSpan], Encoder[TimestampInterval]) extends DisciplineSuite with ArbitraryInstances {

  import ArbTimeSpan.given
  import ArbTimestampInterval.given
  import time.decoder.given

  val MinDate: LocalDate = LocalDate.of(1901,  1,  1)
  val MaxDate: LocalDate = LocalDate.of(2099, 12, 31)

  given Arbitrary[DateInterval] =
    Arbitrary {
      for {
        a <- Gen.choose(MinDate, MaxDate)
        b <- Gen.choose(MinDate, MaxDate)
      } yield DateInterval.between(a, b)
    }

  given Cogen[DateInterval] =
    Cogen[(LocalDate, LocalDate)].contramap(a => (
      a.start,
      a.end
    ))

  checkAll("DateInterval", CodecTests[DateInterval].codec)
  checkAll("TimeSpanCodec", CodecTests[TimeSpan].codec)
  checkAll("TimestampInterval", CodecTests[TimestampInterval].codec)
}

class TimeQuerySuite extends TimeSuite(using
  time.query.given_Encoder_DateInterval,
  time.query.Encoder_TimeSpan,
  time.query.Encoder_TimestampInterval
)

class TimeTransportSuite extends TimeSuite(using
  time.transport.given_Encoder_DateInterval,
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
