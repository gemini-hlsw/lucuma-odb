// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.*

import java.time.Duration
import java.time.temporal.ChronoUnit.MICROS

abstract class TimeSuite(using Encoder[Duration]) extends DisciplineSuite with ArbitraryInstances {

  import time.decoder.given

  // A real arbitrary duration cannot always be encoded in a way that matches
  // the schema.  Only durations that fit in a `Long` with unit microseconds.
  given Arbitrary[Duration] =
    Arbitrary {
      for {
        m <- arbitrary[Long]
        s  = m % 1
        n <- Gen.chooseNum(0L, 999_999_999L)
      } yield {
        val n = (m % 1_000_000).abs * 1_000
        val s = m/1_000_000
        Duration.ofSeconds(s, n)
      }
    }

  given Eq[Duration] =
    Eq.by { d =>
      (d.getSeconds, d.getNano)
    }

  import time.given

  // This won't round-trip with a real arbitrary Duration.
  checkAll("DurationCodec", CodecTests[Duration].codec)
}

class TimeQuerySuite extends TimeSuite(using time.query.Encoder_Duration)

class TimeTransportSuite extends TimeSuite(using time.transport.Encoder_Duration)