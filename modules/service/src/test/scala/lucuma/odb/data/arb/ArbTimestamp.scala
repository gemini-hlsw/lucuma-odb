// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
package arb

import java.time.Duration
import java.time.temporal.ChronoUnit.MICROS
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbTimestamp {

  val arbTimestamp: Arbitrary[Timestamp] =
    Arbitrary {
      Gen.choose[Long](0, ArbTimestamp.MaxDurationNanos).map { µs =>
        Timestamp.unsafeFromInstant(
          Timestamp.Min.toInstant.plusNanos(µs).truncatedTo(MICROS)
        )
      }
    }
}

object ArbTimestamp extends ArbTimestamp {

  private val MaxDuration: Duration =
    Duration.between(Timestamp.Min.toInstant, Timestamp.Max.toInstant)

  private val MaxDurationNanos: Long =
    MaxDuration.toNanos

}
