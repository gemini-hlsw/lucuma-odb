// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
package arb

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import java.time.Duration
import java.time.Instant
import java.time.temporal.ChronoUnit.MICROS

object ArbTimestamp {

  private val MaxDuration: Duration =
    Duration.between(Timestamp.Min.toInstant, Timestamp.Max.toInstant)

  private val MaxDurationMillis: Long =
    MaxDuration.toMillis

  given arbTimestamp: Arbitrary[Timestamp] =
    Arbitrary {
      Gen.choose[Long](0, ArbTimestamp.MaxDurationMillis).map { ms =>
        Timestamp.unsafeFromInstant(
          Timestamp.Min.toInstant.plusMillis(ms)
        )
      }
    }

  given cogTimestamp: Cogen[Timestamp] =
    Cogen[Instant].contramap(_.toInstant)

  val genTimestampString: Gen[String] =
    Gen.oneOf(
      arbitrary[Timestamp].map(_.format),
      arbitrary[Timestamp].map(_.format).map(s => s"${s}000"),
      arbitrary[Timestamp].map(_.isoFormat),
      arbitrary[(Timestamp, Int, Char)].map { case (t, i, c) =>
        val cs = t.format.toCharArray
        val in = (i % cs.size).abs
        cs(in) = c
        String.valueOf(cs)
      }
    )

}
