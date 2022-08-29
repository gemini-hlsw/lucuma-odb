// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order

import munit.FunSuite
import java.time.Instant

class TimestampSuite extends FunSuite {

  test("foo") {

    val t0 = Timestamp.fromInstant(Instant.parse("2022-08-29T15:22:01.000001Z"))
    val t1 = Timestamp.fromInstant(Instant.parse("2022-08-29T15:22:00.000001Z"))
    println(t1.get.toInstant)

    Order[Timestamp].compare(t0.get, t1.get)

  }

}
