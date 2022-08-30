// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.either.*
import cats.syntax.option.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.ZonedDateTime

class TimestampSuite extends ScalaCheckSuite {

  import arb.ArbTimestamp.arbTimestamp

  test("Below Min produces None") {
    assertEquals(Option.empty[Timestamp], Timestamp.fromInstant(Timestamp.Min.toInstant.minusNanos(1000L)))
  }

  test("Above Max produces None") {
    assertEquals(Option.empty[Timestamp], Timestamp.fromInstant(Timestamp.Max.toInstant.plusNanos(1000L)))
  }

  test("Construction at microsecond precision only") {
    val inst = ZonedDateTime.of(2022, 8, 29, 12, 0, 0, 1, UTC).toInstant
    assertEquals(Option.empty[Timestamp], Timestamp.fromInstant(inst))
  }

  test("Parse options") {
    val ts = List(
      Timestamp.parse("1863-07-03 03:00:00"),
      Timestamp.parse("1863-07-03T03:00:00"),
      Timestamp.parse("1863-07-03 03:00:00Z"),
      Timestamp.parse("1863-07-03T03:00:00Z"),
      Timestamp.parse("1863-07-03 03:00:00.0"),
      Timestamp.parse("1863-07-03 03:00:00.00"),
      Timestamp.parse("1863-07-03 03:00:00.000"),
      Timestamp.parse("1863-07-03 03:00:00.0000"),
      Timestamp.parse("1863-07-03 03:00:00.00000"),
      Timestamp.parse("1863-07-03 03:00:00.000000")
    )
    val expected = Instant.parse("1863-07-03T03:00:00Z")

    assert(ts.forall(_.toOption.map(_.toInstant) == Some(expected)))
  }

  test("Parse sub-microsecond fails") {
    val s = "1863-07-03 03:00:00.0000000"
    assertEquals(s"Could not parse as a Timestamp: $s".asLeft[Timestamp], Timestamp.parse(s))
  }

  property("Round-trip parse / format") {
    forAll { (t0: Timestamp) =>
      assertEquals(t0.some, Timestamp.parse(t0.format).toOption)
    }
  }

}
