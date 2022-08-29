// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.option.*
import munit.FunSuite

import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.ZonedDateTime

class TimestampSuite extends FunSuite {

  test("Below Min produces None") {
    assertEquals(Option.empty[Timestamp], Timestamp.fromInstant(Timestamp.Min.toInstant.minusNanos(1L)))
  }

  test("Above Max produces None") {
    assertEquals(Option.empty[Timestamp], Timestamp.fromInstant(Timestamp.Max.toInstant.plusNanos(1L)))
  }

  test("Value is truncated") {
    val precise = ZonedDateTime.of(2022, 8, 29, 12, 0, 0, 1, UTC).toInstant
    val trunc   = precise.minusNanos(1L)
    assertEquals(trunc.some, Timestamp.fromInstant(precise).map(_.toInstant))
  }

}
